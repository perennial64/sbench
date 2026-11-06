package sbench

import scala.annotation.nowarn

/** Input state passed to a benchmark body. */
sealed abstract class Input[S] {
  private[sbench] def get: S
}

object Input {

  /** State built once per benchmark. Correct when the body does not mutate it.
    */
  def perTrial[S](state: S): Input[S] = new PerTrial(state)

  /** State rebuilt before every invocation, for bodies that mutate it. */
  def perInvocation[S](state: => S): Input[S] = new PerInvocation(state)

  /** No state, for bodies that need none. */
  val empty: Input[Unit] = perTrial(())

  final class PerTrial[S] private[sbench] (state: S) extends Input[S] {

    @nowarn("msg=consider using")
    @volatile private var cell: S = state

    private[sbench] def get: S = cell
  }

  final class PerInvocation[S] private[sbench] (state: => S) extends Input[S] {

    private[sbench] def get: S = state
  }
}
