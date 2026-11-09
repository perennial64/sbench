package sbench.examples

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt

import sbench.Bench
import sbench.Input
import sbench.Suite

/** The three ways a sample's invocation count is decided. */
object Batching extends Suite {

  /** The default. Every sample runs the same count, sized after warmup so that
    * one sample takes about `sampleTime` (10 millis here, 25 by default).
    */
  bench("timed sizing", warmup = 200.millis, samples = 10, batch = Bench.Batch.Timed(10.millis)) {
    for { seed <- Input.perTrial(1) } yield seed * 2654435761L
  }

  /** Counts ramp across the samples, so that the per-invocation cost can be
    * fitted as a slope instead of carrying each sample's fixed cost. Only sound
    * when every invocation costs the same.
    */
  bench("ramped fit", warmup = 200.millis, samples = 20, batch = Bench.Batch.Ramped(10.millis)) {
    for { seed <- Input.perTrial(1) } yield seed * 2654435761L
  }

  /** A pinned count, which is what a body with no steady state needs. This one
    * grows its buffer with every append, so sizing by time would measure a
    * different stretch of that growth each run. Paired with per-batch state so
    * every sample starts from an empty buffer, the result reads as the mean
    * over the first 100,000 appends.
    */
  bench(
    "appending to a buffer",
    warmup = 200.millis,
    samples = 10,
    batch = Bench.Batch.Fixed(100_000),
  ) {
    for { buffer <- Input.perBatch(ListBuffer.empty[Int]) } yield buffer += 1
  }
}
