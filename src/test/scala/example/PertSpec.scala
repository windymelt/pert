package pert

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  val network17 = Network(
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

  "Network" should "pass 17-B" in {
    network17.earlyNodeTimes shouldEqual Map(
      1 -> 0.0,
      2 -> 8.0,
      3 -> 5.0,
      4 -> 18.0,
      5 -> 18.0,
      6 -> 23.0,
      7 -> 30.0
    )
  }

  "Network" should "pass 17-C" in {
    network17.lateNodeTimes shouldEqual Map(
      1 -> 0.0,
      2 -> 8.0,
      3 -> 9.0,
      4 -> 18.0,
      5 -> 21.0,
      6 -> 26.0,
      7 -> 30.0
    )
  }

  "Network" should "pass 18" in {
    network17.totalFloat shouldEqual Map(
      1 -> 2 -> 0.0,
      1 -> 3 -> 4.0,
      1 -> 5 -> 9.0,
      2 -> 4 -> 0.0,
      2 -> 7 -> 14.0,
      3 -> 4 -> 4.0,
      4 -> 5 -> 3.0, // dummy activity
      4 -> 7 -> 0.0,
      5 -> 6 -> 3.0,
      6 -> 7 -> 3.0
    )
  }

  val network19 = Network(
    Set(
      Activity(1, 2, 1),
      Activity(1, 4, 10),
      Activity(1, 3, 5),
      Activity(2, 3, 3),
      Activity(3, 4, 2)
    )
  )

  "Network" should "pass 20" in {
    network19.totalFloat shouldEqual Map(
      1 -> 2 -> 4.0,
      1 -> 3 -> 3.0,
      1 -> 4 -> 0.0,
      2 -> 3 -> 4.0,
      3 -> 4 -> 3.0
    )

    network19.freeFloat shouldEqual Map(
      1 -> 2 -> 0.0,
      1 -> 3 -> 0.0,
      1 -> 4 -> 0.0,
      2 -> 3 -> 1.0,
      3 -> 4 -> 3.0
    )
  }

  "findPath" should "find critical path" in {
    network17.findPath(
      7,
      1,
      (i) => (j) => network17.totalFloat(i -> j) == 0.0
    ) shouldEqual Set(
      Seq(1, 2, 4, 7)
    )
  }

  "criticalPath" should "find critical path" in {
    network17.criticalPaths shouldEqual Set(
      Seq(1, 2, 4, 7)
    )
  }
}
