package sbench.examples

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.DurationInt

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Running one body on several threads, for bodies whose behaviour under
  * contention is the point.
  */
object Threads extends Suite {

  /** Scala.js has no threads, and a Scala Native build only has them when it
    * was linked with multithreading.
    */
  private val threads: Int = math.min(4, Bench.cores)

  /** One counter for every thread: a cache line that every core writes. */
  bench("shared counter", warmup = 200.millis, samples = 10, threads = threads) {
    for { shared <- Input.perTrial(new AtomicLong(0)) } yield shared.incrementAndGet()
  }

  /** A counter each, which is the same work without the contention. The gap
    * between the two is what `threads` is for.
    */
  bench("counter per thread", warmup = 200.millis, samples = 10, threads = threads) {
    for { own <- Input.perThread(new AtomicLong(0)) } yield own.incrementAndGet()
  }
}
