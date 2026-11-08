package sbench.examples

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Bench that declares how much work one invocation does, so the report can
  * state throughput as well as time.
  */
object ChecksumBench extends Suite {

  private val payload: Array[Byte] = Array.tabulate(1 << 20)(i => (i * 31).toByte)

  bench("fletcher32 of 1MiB", Bench.Options(work = Bench.Work.Bytes(payload.length.toLong))) {
    for {
      bytes <- Input.perTrial(payload)
    } yield {
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

  bench("sum of 1M ints, per element", Bench.Options(work = Bench.Work.Ops(1000000L))) {
    for {
      numbers <- Input.perTrial(Array.range(0, 1_000_000))
    } yield numbers.sum
  }
}
