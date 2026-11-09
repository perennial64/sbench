package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

import scala.scalajs.js

private[sbench] object Platform {

  final val name = "js"

  def env(key: String): Option[String] =
    if (js.typeOf(js.Dynamic.global.process) == "undefined") None
    else {
      val value = js.Dynamic.global.process.env.selectDynamic(key)
      if (js.typeOf(value) == "string") Some(value.asInstanceOf[String]) else None
    }

  /** Whether more than one thread can run here.
    *
    * Never: web workers do not share a heap.
    */
  val supportsThreads: Boolean = false

  /** Hardware threads available, which is one here. */
  val cores: Int = 1

  /** Runs the only worker there can be, [[maxThreads]] being one. */
  def runWorkers(count: Int)(worker: Int => Unit): Unit =
    if (count == 1) worker(0)
    else throw new UnsupportedOperationException("this platform runs a single thread")

  /** V8 tiers up through Sparkplug and Maglev to TurboFan. */
  val warmup: FiniteDuration = 1.second
}
