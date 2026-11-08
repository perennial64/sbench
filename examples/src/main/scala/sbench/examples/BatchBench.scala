package sbench.examples

import scala.collection.mutable.ListBuffer

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Benches that need batching, since one invocation of either body is far too
  * short.
  */
object BatchBench extends Suite {

  bench("xorshift mix", Bench.Options()) {
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

  bench("append to buffer", Bench.Options(batch = Bench.Batch.Fixed(100_000))) {
    Input.perBatch(ListBuffer.empty[Int]).map(buf => buf += 1)
  }
}
