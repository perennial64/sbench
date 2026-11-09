package sbench

import scala.annotation.nowarn

/** Blackhole sink instance.
  *
  * Intended to be acquired by single-threaded benchmark bodies via
  * [[sbench.Input.blackhole]] accessor. Using `consume` accross threads
  * concurrently may lead up to 19x slowdown compared to single threaded use.
  */
final class Blackhole private[sbench] () {

  @nowarn private var objSink: Any = null
  @nowarn private var longSink: Long = 0L
  @nowarn private var doubleSink: Double = 0.0

  /** Volatile guard used for always-true if branch in `consume` bodies. Avoids
    * costly volatile write while preventing complete body elision.
    */
  @nowarn @volatile private var guard: Int = 1

  /** Make use of an `x` in a way that wouldn't be optimized away at runtime. */
  def consume(x: Any): Unit = if ((guard & 1) != 0) objSink = x
  def consume(x: Long): Unit = if ((guard & 1) != 0) longSink = x
  def consume(x: Int): Unit = if ((guard & 1) != 0) longSink = x.toLong
  def consume(x: Double): Unit = if ((guard & 1) != 0) doubleSink = x
  def consume(x: Boolean): Unit = if ((guard & 1) != 0) longSink = if (x) 1L else 0L
}
