package sbench.examples

import scala.concurrent.duration.DurationInt

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Declaring how much work one invocation does, so a report can state a rate as
  * well as a time.
  */
object Throughput extends Suite {

  private val payload: Array[Byte] = Array.tabulate(1 << 20)(i => (i * 31).toByte)

  /** Bytes per invocation, reported in binary units. */
  bench(
    "fletcher32 of 1MiB",
    warmup = 200.millis,
    samples = 10,
    work = Bench.Work.Bytes(payload.length.toLong),
  ) {
    for { bytes <- Input.perTrial(payload) } yield {
      var sum1 = 0xffff
      var sum2 = 0xffff
      var i = 0
      while (i < bytes.length) {
        sum1 = (sum1 + (bytes(i) & 0xff)) % 0xffff
        sum2 = (sum2 + sum1) % 0xffff
        i += 1
      }
      (sum2 << 16) | sum1
    }
  }

  /** Logical operations per invocation, reported in SI units. */
  bench(
    "sum of 1M ints, per element",
    warmup = 200.millis,
    samples = 10,
    work = Bench.Work.Ops(1_000_000),
  ) {
    for { numbers <- Input.perTrial(Array.range(0, 1_000_000)) } yield numbers.sum
  }
}
