package sbench.examples

import scala.concurrent.duration.DurationInt

import sbench.Input
import sbench.Suite

/** Demonstration of basic benchmark use cases. */
object Basics extends Suite {

  /** Warmup specifies for how long to rerun the benchmark body before
    * measuring, to let the JIT and the caches settle. Defaults to 3 seconds on
    * the JVM, 1 second on JS and 250 millis on Native (branch predictors,
    * static initializators).
    *
    * Samples specify how many timed measurements to keep. Each one covers a
    * batch of invocations rather than a single one, so the body runs far more
    * often than this. Defaults to 30.
    */
  bench("sum of 1M ints", warmup = 200.millis, samples = 10) {
    for {
      // Inputs are declared in a for comprehension. This one is built once
      // and shared by every invocation.
      //
      // Taking state as a parameter rather than closing over it keeps the
      // compiler from folding it into a constant, and composes when a body
      // needs several inputs rebuilt at different rates.
      numbers <- Input.perTrial(Array.range(0, 1_000_000))
    } yield
      // The timed body is the yield.
      numbers.sum
  }

  /** A body far too short to time on its own gets automatically batched at
    * runtime and the cost of one is reported.
    */
  bench("xorshift mix", warmup = 200.millis, samples = 10) {
    for {
      seed <- Input.perTrial(123456789)
    } yield {
      var x = seed
      x ^= x << 13
      x ^= x >>> 17
      x ^= x << 5
      x
    }
  }
}
