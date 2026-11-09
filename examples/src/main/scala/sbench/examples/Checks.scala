package sbench.examples

import scala.concurrent.duration.DurationInt

import sbench.Bench
import sbench.Input
import sbench.Suite

/** What a benchmark that goes wrong looks like.
  *
  * A failing check, or a body that throws, is reported in place of that one
  * result and does not stop the rest of the suite.
  */
object Checks extends Suite {

  /** `verify` states what a body should produce. It runs once, outside any
    * measurement, so it costs nothing in the timed loop.
    */
  bench("wrong expectation", warmup = 0.millis, samples = 2, batch = Bench.Batch.Fixed(1)) {
    (for { n <- Input.perTrial(2) } yield n + 2).verify("sums to five")(_ == 5)
  }

  bench("throwing body", warmup = 0.millis, samples = 2, batch = Bench.Batch.Fixed(1)) {
    for { n <- Input.perTrial(0) } yield 1 / n
  }
}
