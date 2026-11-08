package sbench

import scala.collection.mutable.ListBuffer

/** Contaner of a benchmark suite. */
trait Suite {

  private final class Entry[S](val name: String, input: Input[S], opts: Bench.Options, body: S => Any) {
    def run(): Bench.Result = Bench.bench(input, opts)(body)
  }

  private val benches = ListBuffer.empty[Entry[_]]

  /** Append benchmark over given input. */
  final def bench[S](
      name: String,
      input: Input[S],
      opts: Bench.Options = Bench.Options(),
  )(body: S => Any): Unit =
    benches += new Entry(name, input, opts, body)

  /** Append benchmark that needs no input. */
  final def bench(name: String, opts: Bench.Options)(body: => Any): Unit =
    bench(name, Input.empty, opts)(_ => body)

  /** Append benchmark that needs no input, with default options. */
  final def bench(name: String)(body: => Any): Unit =
    bench(name, Input.empty, Bench.Options())(_ => body)

  /** Run all configure benchmarks and print result to stdout. */
  final def run(): Unit =
    for (entry <- benches) {
      val result = entry.run()
      println(
        s"${entry.name}, runs=${result.runs}, batch=${result.batch}, " +
          s"mean=${math.round(result.meanNs)}ns, min=${math.round(result.minNs)}ns, " +
          s"max=${math.round(result.maxNs)}ns"
      )
    }
}
