package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

private[sbench] object Platform {

  final val name = "jvm"

  def env(key: String): Option[String] = sys.env.get(key)

  /** HotSpot climbs the interpreter, then C1, then C2, and only inlines once a
    * call site is hot.
    */
  val warmup: FiniteDuration = 3.seconds
}
