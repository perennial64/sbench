package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import scala.scalanative.meta.LinktimeInfo

private[sbench] object Platform {

  final val name = "native"

  def env(key: String): Option[String] = sys.env.get(key)

  /** Whether more than one thread can run here, which needs the build to have
    * enabled multithreading.
    */
  val supportsThreads: Boolean = LinktimeInfo.isMultithreadingEnabled

  /** Hardware threads available. */
  val cores: Int = Runtime.getRuntime.availableProcessors()

  /** Run `count` workers concurrently, returning once all have finished. */
  def runWorkers(count: Int)(worker: Int => Unit): Unit =
    if (!LinktimeInfo.isMultithreadingEnabled) {
      if (count == 1) worker(0)
      else throw new UnsupportedOperationException("this build runs a single thread")
    } else {
      val threads = Array.tabulate(count) { index =>
        new Thread(
          new Runnable {
            def run(): Unit = worker(index)
          },
          "sbench-" + index,
        )
      }
      threads.foreach(_.start())
      threads.foreach(_.join())
    }

  /** Fill caches and train branch predictors. */
  val warmup: FiniteDuration = 250.millis
}
