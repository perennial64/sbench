package sbench.examples

import scala.collection.mutable.ListBuffer

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Benches that need batching, since one invocation of either body is far too
  * short.
  */
object BatchBench extends Suite {

  bench(
    "xorshift mix",
    Input.perTrial(123456789),
    Bench.Options(runs = 10, warmup = 3, batch = 1_000_000),
  ) { seed =>
    var x = seed
    x ^= x << 13
    x ^= x >>> 17
    x ^= x << 5
    x
  }

  bench(
    "append to buffer",
    Input.perBatch(ListBuffer.empty[Int]),
    Bench.Options(runs = 10, warmup = 3, batch = 100_000),
  ) { buf =>
    buf += 1
  }
}
