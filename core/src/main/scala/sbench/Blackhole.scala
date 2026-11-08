package sbench

import scala.annotation.nowarn

object Blackhole {

  @nowarn @volatile private var sink: Any = null

  /** Make use of `x` in a way that wouldn't be optimized away by the compiler
    * as unused.
    */
  def consume(x: Any): Unit =
    sink = x
}
