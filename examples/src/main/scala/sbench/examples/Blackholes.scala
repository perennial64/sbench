package sbench.examples

import scala.concurrent.duration.DurationInt

import sbench.Input
import sbench.Suite

/** Keeping alive the values a body computes along the way.
  *
  * A body's result is consumed by default, so most benchmarks never need a
  * blackhole. One is needed when a body computes several things worth keeping.
  * Packing them into a tuple to return them together may read less clearly, and
  * the packing itself lands inside the timed region, even though what that
  * costs varies by platform. For small values it is mostly a stylistic choice.
  *
  * A blackhole is taken at the thread level, so a benchmark running on several
  * threads gets one each. A single sink written from several threads leads to
  * cache line contention, which a benchmark would then be measuring instead.
  */
object Blackholes extends Suite {

  /** Two results, with one consumed through a blackhole and the other returned.
    */
  bench("sunk results", warmup = 200.millis, samples = 10) {
    for {
      numbers <- Input.perTrial(Array.range(0, 8))
      bh <- Input.blackhole
    } yield {
      var least = Int.MaxValue
      var most = Int.MinValue
      var i = 0
      while (i < numbers.length) {
        val value = numbers(i) * 2654435761L
        if (value < least) least = value.toInt
        if (value > most) most = value.toInt
        i += 1
      }
      bh.consume(least)
      most
    }
  }

  /** The same work, but packed into a tuple to return them both. */
  bench("tupled results", warmup = 200.millis, samples = 10) {
    for {
      numbers <- Input.perTrial(Array.range(0, 8))
    } yield {
      var least = Int.MaxValue
      var most = Int.MinValue
      var i = 0
      while (i < numbers.length) {
        val value = numbers(i) * 2654435761L
        if (value < least) least = value.toInt
        if (value > most) most = value.toInt
        i += 1
      }
      (least, most)
    }
  }
}
