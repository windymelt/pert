package pert

case class Activity(from: Int, to: Int, cost: Float, name: String = "") {
  val isDummy = cost == 0
}

case class Network(
    activities: Set[Activity],
    forceEndTime: Option[Float] = None
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

    intermediateNodeTimes(endEvent) =
      forceEndTime.getOrElse(earlyNodeTimes(endEvent))
    val startCost = calcLateTimesByStartEvent(startEvent, intermediateNodeTimes)
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
      intermediateNodeTimes: MuMap[Event, Float]
  ): Float = {
    val activitiesToEvent = activities.filter(_.from == event)
    val minCost = activitiesToEvent.map { a =>
      val startCost = intermediateNodeTimes.getOrElseUpdate(
        a.to,
        calcLateTimesByStartEvent(a.to, intermediateNodeTimes)
      )

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
        val label = s"${e}\\n${earlyNodeTimes(e)}..${lateNodeTimes(e)}"
        s"Ev${e} [ shape = circle, label = ${'"'}${label}${'"'} ];"
      }
      .mkString("\n")
    val minimumTotalFloat = totalFloat.minBy(_._2)._2
    activities.map { a =>
      val tf = totalFloat(a.from -> a.to)
      val color = if (tf <= 0 && tf == minimumTotalFloat) { "color = red, " }
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
  implicit def tuple3ToActivity[A <% Float](
      tuple: Tuple3[Int, Int, A]
  ): Activity =
    Activity(tuple._1, tuple._2, tuple._3)
  implicit def tuple4ToActivity[A <% Float](
      tuple: Tuple4[Int, Int, A, String]
  ): Activity =
    Activity(tuple._1, tuple._2, tuple._3, tuple._4)
  val network = Network(
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

  val net2 = Network(
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
    Some(41)
  )
  println(net2.dot)
}
