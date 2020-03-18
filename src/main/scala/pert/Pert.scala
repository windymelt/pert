package pert
import scala.collection.immutable.Nil
import com.github.nscala_time.time.Imports._
import org.joda.time.Days

sealed trait ActivityStatus
final object ActivityStatus {
  def apply(str: String): ActivityStatus = str match {
    case "done" => Done
    case "wip" => WIP
    case "waiting" | "" => Waiting
  }
}
final object Done extends ActivityStatus
final object WIP extends ActivityStatus
final object Waiting extends ActivityStatus

case class Activity(from: Int, to: Int, cost: Float, name: String = "", status: ActivityStatus = Waiting) {
  val isDummy = cost == 0
}

case class Network(
    activities: Set[Activity],
    deadlineCostMap: Map[Int, Float] = Map(),
) {
  import scala.collection.mutable.{Seq => MuSeq, Map => MuMap, Set => MuSet}
  type Event = Int
  type Cost = Float
  type FloatType = Float

  lazy val earlyNodeTimes: Map[Event, Cost] = {
    val intermediateNodeTimes = MuMap[Event, Cost]()
    val endEvent = findEndEvent
    val startEvent = findStartEvent

    intermediateNodeTimes(startEvent) = 0
    val endCost = calcEarlyTimesByEndEvent(endEvent, intermediateNodeTimes)
    intermediateNodeTimes(endEvent) = endCost

    intermediateNodeTimes.toMap
  }

  lazy val lateNodeTimes: Map[Event, Cost] = {
    val intermediateNodeTimes = MuMap[Event, Cost]()
    val endEvent = findEndEvent
    val startEvent = findStartEvent

    deadlineCostMap.map { kv => 
      intermediateNodeTimes(kv._1) = kv._2
    }
    intermediateNodeTimes.getOrElseUpdate(endEvent, earlyNodeTimes(endEvent))
    val startCost = calcLateTimesByStartEvent(startEvent, intermediateNodeTimes, endEvent)
    intermediateNodeTimes(startEvent) = startCost

    intermediateNodeTimes.toMap
  }

  lazy val totalFloat: Map[Tuple2[Event, Event], FloatType] = {
    activities.map { a =>
      val float = lateNodeTimes(a.to) - (earlyNodeTimes(a.from) + a.cost)
      a.from -> a.to -> float
    }.toMap
  }

  lazy val freeFloat: Map[Tuple2[Event, Event], FloatType] = {
    activities.map { a =>
      val float = earlyNodeTimes(a.to) - (earlyNodeTimes(a.from) + a.cost)
      a.from -> a.to -> float
    }.toMap
  }

  lazy val criticalPaths: Set[Seq[Event]] =
    findPath(
      findEndEvent,
      findStartEvent,
      from => to => totalFloat(from -> to) == 0.0
    )

  def findPath(
      currentEvent: Event,
      targetEvent: Event,
      pred: Event => Event => Boolean = _ => _ => true,
      path: Seq[Event] = Seq()
  ): Set[Seq[Event]] = {
    val availableActivities = // 外に出せる
      activities.filter(a => a.to == currentEvent && pred(a.from)(a.to))
    val finishActivity = availableActivities.find(_.from == targetEvent) match {
      case None        => Set()
      case Some(value) => Set(Seq(targetEvent, currentEvent) ++ path)
    }
    val otherPaths = availableActivities
      .filterNot(_.from == targetEvent)
      .flatMap(a => findPath(a.from, targetEvent, pred, currentEvent +: path))
      .toSet

    otherPaths ++ finishActivity
  }

  private def calcEarlyTimesByEndEvent(
      event: Event,
      intermediateNodeTimes: MuMap[Event, Cost]
  ): Float = {
    val activitiesToEvent = activities.filter(_.to == event)
    val maxCost = activitiesToEvent.map { a =>
      val lastCost = intermediateNodeTimes.getOrElseUpdate(
        a.from,
        calcEarlyTimesByEndEvent(a.from, intermediateNodeTimes)
      )

      lastCost + a.cost
    }.max

    maxCost
  }

  private def calcLateTimesByStartEvent(
      event: Event,
      intermediateNodeTimes: MuMap[Event, Float],
      endEvent: Event
  ): Float = {
    val activitiesToEvent = activities.filter(_.from == event)
    if (activitiesToEvent.isEmpty) {

      throw new Exception(s"There is no activity which comes from [${event}]")
    }
    val minCost = activitiesToEvent.map { a =>
      val startCost = a.to match {
        case `endEvent` =>
          intermediateNodeTimes(
            a.to
          )
        case _ =>
          val computed = calcLateTimesByStartEvent(a.to, intermediateNodeTimes, endEvent) // 既に値があっても，全てのノードを計算する必要はあるので計算は続ける
          intermediateNodeTimes.getOrElseUpdate(
            a.to,
            computed
          )
      }

      startCost - a.cost
    }.min

    minCost
  }

  val findStartEvent: Event = {
    val allEvent: scala.collection.mutable.Map[Int, Boolean] =
      scala.collection.mutable.Map(
        // automatically distinct because `activities` is Set
        activities.flatMap(a => Seq(a.from, a.to)).map(_ -> true).toSeq: _*
      )
    activities.foreach(a => {
      allEvent.-=(a.to)
    })
    allEvent.keys.head
  }

  val findEndEvent: Event = {
    val allEvent: scala.collection.mutable.Map[Int, Boolean] =
      scala.collection.mutable.Map(
        // automatically distinct because `activities` is Set
        activities.flatMap(a => Seq(a.from, a.to)).map(_ -> true).toSeq: _*
      )
    activities.foreach(a => {
      allEvent.-=(a.from)
    })
    allEvent.keys.head
  }

  def dot: String = {
    val header = """
graph [
rankdir = "LR"
];
    """
    val nodes = earlyNodeTimes.keys
      .map { e =>
        val label = s"${e}\\n${earlyNodeTimes.getOrElse(e, "N/A")}..${lateNodeTimes.getOrElse(e, "N/A")}"
        s"Ev${e} [ shape = circle, label = ${'"'}${label}${'"'} ];"
      }
      .mkString("\n")
    val minimumTotalFloat = totalFloat.minBy(_._2)._2
    activities.map { a =>
      val tf = totalFloat(a.from -> a.to)
      val color = if (a.status == Done) {
        "color = \"green\", "
      } else if (a.status == WIP) {
        "color = \"yellow\", "
      } else if (tf < 0 && tf == minimumTotalFloat) {
        "color = \"red:invis:red\", "
      } else if (tf < 0) { "color = red, " }
      else { "" }
      val style = if (a.isDummy) { "style = dashed, " }
      else if (totalFloat(a.from -> a.to) <= 0.0) { "style = bold, " }
      else { "" }
      s"Ev${a.from} -> Ev${a.to} [ ${style}${color}label = ${'"'}${a.name}(${a.cost})\\n[T:${totalFloat(
        a.from -> a.to
      )}/F:${freeFloat(a.from -> a.to)}]${'"'} ];"
    } mkString (s"digraph PERT {\n${header}\n${nodes}\n", "\n", "\n}\n")
  }
}
case class DetailedActivity(from: Int, to: Int, cost: Float)

case class DetailedNetwork(activities: Seq[DetailedActivity])

object Hello extends App {
  import com.github.tototoshi.csv._
  import java.io.File

  implicit def tuple3ToActivity[A <% Float](
      tuple: Tuple3[Int, Int, A]
  ): Activity =
    Activity(tuple._1, tuple._2, tuple._3)
  implicit def tuple4ToActivity[A <% Float](
      tuple: Tuple4[Int, Int, A, String]
  ): Activity =
    Activity(tuple._1, tuple._2, tuple._3, tuple._4)
  val network1 = Network(
    Set(
      (1, 2, 8),
      (1, 3, 5),
      (1, 5, 12),
      (2, 7, 8),
      (2, 4, 10),
      (3, 4, 9),
      (4, 7, 12),
      (4, 5, 0),
      (5, 6, 5),
      (6, 7, 4)
    )
  )

  val network2 = Network(
    Set(
      (1, 2, 5),
      (1, 3, 15),
      (2, 5, 10),
      (2, 4, 2),
      (3, 11, 7),
      (4, 5, 0),
      (4, 6, 4),
      (5, 6, 10),
      (5, 7, 12),
      (6, 8, 6),
      (6, 9, 2),
      (7, 8, 7),
      (8, 10, 12),
      (9, 10, 10),
      (10, 12, 2),
      (11, 12, 19)
    ),
    Map(12 -> 41)
  )

  val network = args.toList match {
    case Nil =>
      println(network2.dot)
    case head :: tl =>
      val csvReader = CSVReader.open(new File(head))
      val activities = scala.collection.mutable.Set[Activity]()
      val deadLineDict = scala.collection.mutable.Map[Int, Float]()
      csvReader.toStreamWithHeaders.foreach { kvmap =>
        kvmap.get("status").map { status =>
          activities += Activity(
            kvmap("from").toInt,
            kvmap("to").toInt,
            kvmap("cost").toFloat,
            kvmap("name"),
            ActivityStatus(status)
          )
        } getOrElse {
          activities += Activity(
            kvmap("from").toInt,
            kvmap("to").toInt,
            kvmap("cost").toFloat,
            kvmap("name"),
          )
        } 
        val now = DateTime.now()
        kvmap("deadline") match {
          case "" => // nop
          case deadline =>
            val parsedDT = DateTime.parse(deadline)
            val deadlineDays = Days.daysBetween(now, parsedDT).getDays()
            println(s"deadline detection: event [${kvmap("to")}] deadline ${parsedDT} -> ${deadlineDays}")
            deadLineDict.get(kvmap("to").toInt) match {
              case None => deadLineDict += kvmap("to").toInt -> deadlineDays
              case Some(dt) => if (deadlineDays < dt) { deadLineDict(kvmap("to").toInt) = deadlineDays }
            }
        }
      }
      csvReader.close()
      val network = Network(activities.toSet, deadLineDict.toMap)
      val write = new java.io.FileWriter(new File(head + ".dot"))
      write.write(network.dot)
      write.close()
  }
}
