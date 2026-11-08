package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

private[sbench] object Platform {

  final val name = "native"

  def env(key: String): Option[String] = sys.env.get(key)

  /** Fill caches and train branch predictors. */
  val warmup: FiniteDuration = 250.millis
}
