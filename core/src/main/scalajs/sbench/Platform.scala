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

  /** V8 tiers up through Sparkplug and Maglev to TurboFan. */
  val warmup: FiniteDuration = 1.second
}
