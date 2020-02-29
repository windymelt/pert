package example

case class Activity(from: Int, to: Int, cost: Float, name: String = "") {
  val isDummy = cost == 0

  def toDotEdge = s"Event${from} -> Event${to};"
}

case class Network(activities: Set[Activity]) {
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

    intermediateNodeTimes(endEvent) = earlyNodeTimes(endEvent)
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
    println(s"finish: ${finishActivity}")
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
    activities.map { a =>
      s"${a.from} -> ${a.to} [ ${if (a.isDummy) "style = dashed, "
      else if (totalFloat(a.from -> a.to) == 0.0) "style = bold, "
      else ""}label = ${'"'}${a.name}[${a.cost}]${'"'} ];"
    } mkString (s"digraph PERT {\n${header}\n", "\n", "\n}\n")
  }
}
case class DetailedActivity(from: Int, to: Int, cost: Float)

case class DetailedNetwork(activities: Seq[DetailedActivity])

object Hello extends App {
  val network = Network(
    Set(
      Activity(1, 2, 8),
      Activity(1, 3, 5),
      Activity(1, 5, 12),
      Activity(2, 7, 8),
      Activity(2, 4, 10),
      Activity(3, 4, 9),
      Activity(4, 7, 12),
      Activity(4, 5, 0),
      Activity(5, 6, 5),
      Activity(6, 7, 4)
    )
  )

  println(network.dot)
}
