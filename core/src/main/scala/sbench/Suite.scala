package sbench

import scala.collection.mutable.ListBuffer

/** Contaner of a benchmark suite. */
trait Suite {

  private val benches = ListBuffer.empty[(String, Bench.Options, () => Unit)]

  /** Append benchmark with specified options. */
  final def bench(name: String, opts: Bench.Options)(body: => Unit): Unit =
    benches += ((name, opts, () => body))

  /** Run all configure benchmarks and print result to stdout. */
  final def run(): Unit =
    for ((name, opts, body) <- benches) {
      val result = Bench.bench(opts)(body())
      println(
        s"$name, runs=${result.runs}, mean=${math.round(result.meanNs)}ns, " +
          s"min=${result.minNs}ns, max=${result.maxNs}ns"
      )
    }
}
