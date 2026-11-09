package sbench.examples

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Composing inputs that are rebuilt at different rates.
  *
  * Each level says when its state is rebuilt, and inputs are introduced
  * coarsest first.
  *
  * Unlike the coarser levels, perInvocation state is built inside the timed
  * region, and a throwing body skips its release. That cannot be helped: the
  * clock is read once per batch, while the state is rebuilt once per
  * invocation, and reading the clock around every invocation instead would cost
  * more than many bodies do and perturb the very loop it is meant to measure.
  * So reach for this level only when building the state is cheap next to the
  * body, or when the body cannot be benched any other way.
  */
object State extends Suite {

  /** Several levels at once, with the finer ones derived from the coarser:
    *
    *   - the table is expensive and only read, so it is built once
    *   - `width` is derived from that, also once
    *   - the sink grows with every append, so each batch starts over
    *   - the scratch array has to start out blank for every invocation
    */
  bench(
    "table lookup into a buffer",
    warmup = 200.millis,
    samples = 10,
    batch = Bench.Batch.Fixed(50_000),
  ) {
    for {
      table <- Input.perTrial(Array.tabulate(1 << 16)(i => (i * 2654435761L) & 0xffffL))
      width <- Input.perTrial(4)
      sink <- Input.perBatch(ListBuffer.empty[Long])
      scratch <- Input.perInvocation(new Array[Long](width))
    } yield {
      val mask = table.length - 1
      var i = 0
      while (i < scratch.length) {
        scratch(i) = table((sink.size + i) & mask)
        i += 1
      }
      sink += scratch(0) + scratch(width - 1)
      sink.size
    }
  }

  /** State that has to be given back takes a release, which runs outside the
    * timed region for every level but the finest.
    */
  bench("released state", warmup = 100.millis, samples = 5) {
    for {
      pooled <- Input.perBatch(ListBuffer.fill(64)(0), (buffer: ListBuffer[Int]) => buffer.clear())
    } yield pooled.length
  }

  /** A check runs once, outside any measurement, and is dropped from the timed
    * path afterwards. It is how a benchmark proves it measures what it means
    * to: a body whose result is never checked can be optimized away to nothing
    * and still report a plausible number.
    */
  bench("verified sum", warmup = 100.millis, samples = 5) {
    (for {
      numbers <- Input.perTrial(Array.range(0, 1000))
    } yield numbers.sum).verify("sums to 499500")(_ == 499500)
  }
}
