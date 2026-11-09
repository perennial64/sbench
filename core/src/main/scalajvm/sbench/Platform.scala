package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

private[sbench] object Platform {

  final val name = "jvm"

  def env(key: String): Option[String] = sys.env.get(key)

  /** Whether more than one thread can run here. */
  val supportsThreads: Boolean = true

  /** Hardware threads available.
    */
  val cores: Int = Runtime.getRuntime.availableProcessors()

  /** Run `count` workers concurrently, returning once all have finished. */
  def runWorkers(count: Int)(worker: Int => Unit): Unit = {
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

  /** HotSpot climbs the interpreter, then C1, then C2, and only inlines once a
    * call site is hot.
    */
  val warmup: FiniteDuration = 3.seconds
}
