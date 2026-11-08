package sbench.examples

import scala.collection.mutable.ListBuffer

import sbench.Bench
import sbench.Input
import sbench.Suite

/** All three input levels in one benchmark, each rebuilt at its own cadence. */
object LookupBench extends Suite {

  bench("table lookup into buffer", Bench.Options(batch = Bench.Batch.Fixed(50_000))) {
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
}
