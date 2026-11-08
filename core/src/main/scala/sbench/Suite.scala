package sbench

import scala.collection.mutable.ListBuffer

/** Contaner of a benchmark suite. */
trait Suite {

  private final class Entry[L <: Input.Lvl, A](val name: String, chain: Input[L, A], opts: Bench.Options) {
    def run(): Bench.Result = Bench.bench(chain, opts)
  }

  private val benches = ListBuffer.empty[Entry[_, _]]

  /** Append a benchmark over a single input. */
  final def bench[L <: Input.Lvl, S, A](name: String, opts: Bench.Options, input: Input[L, S])(body: S => A): Unit =
    bench(name, opts)(input.map(body))

  /** Append a benchmark, written as a `for` comprehension over its inputs. */
  final def bench[L <: Input.Lvl, A](name: String, opts: Bench.Options = Bench.Options())(chain: Input[L, A]): Unit =
    benches += new Entry(name, chain, opts)

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
