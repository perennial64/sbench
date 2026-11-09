package sbench

import scala.util.Try
import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable.ListBuffer

trait Suite {

  private final class Entry[L <: Input.Lvl, A](val name: String, chain: Input[L, A], opts: Bench.Options) {
    def run(): Bench.Result = Bench.bench(chain, opts)
  }

  private val benches = ListBuffer.empty[Entry[_, _]]

  private var collecting = false

  /** Append a benchmark for this suite to run.
    *
    * @param name
    *   Name to display in report and filter against
    * @param opts
    *   Manually specified benchmark options
    */
  protected final def bench[L <: Input.Lvl, A](name: String, opts: Bench.Options)(chain: Input[L, A]): Unit = {
    require(!collecting, s"cannot register $name: this suite is already collecting")
    require(!benches.exists(_.name == name), s"duplicate benchmark name: $name")
    benches += new Entry(name, chain, opts)
  }

  /** Append a benchmark for this suite to run.
    *
    * @param name
    *   Name to display in report and filter against
    * @param warmup
    *   Time for which bench body would rerun until being considered warmed up
    *   (JIT, branch predictors) and timed
    * @param samples
    *   How many timed samples to keep, which the estimate and its interval are
    *   drawn from
    * @param threads
    *   How many threads run the body at once, for bodies whose behaviour under
    *   contention is the point
    * @param batch
    *   Which batching strategy to use
    * @param work
    *   How to measure throughput, if desired
    */
  protected final def bench[L <: Input.Lvl, A](
      name: String,
      warmup: FiniteDuration = Bench.Defaults.warmup,
      samples: Int = Bench.Defaults.samples,
      threads: Int = Bench.Defaults.threads,
      batch: Bench.Batch = Bench.Defaults.batch,
      work: Bench.Work = Bench.Defaults.work,
  )(chain: Input[L, A]): Unit =
    bench(name, Bench.Options(warmup, samples, threads, batch, work))(chain)

  /** Names of every registered benchmark, in registration order. */
  final def names: Vector[String] = benches.toVector.map(_.name)

  /** Freeze bench registration for this suite and construct lazy iterator over
    * benchmark results.
    *
    * @param filter
    *   Name filter to use
    */
  final def collect(filter: String => Boolean = _ => true): Iterator[(String, Try[Bench.Result])] = {
    collecting = true

    benches.toVector.iterator
      .filter(e => filter(e.name))
      .map(e => (e.name, Try(e.run())))
  }
}
