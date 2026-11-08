package sbench

/** Typed state passed to a benchmark body. */
sealed abstract class Input[S](state: => S) {

  /** State for a benchmark invocation. */
  private[sbench] def get: S = state
}

object Input {

  /** State built once per benchmark. */
  def perTrial[S](state: S): Input[S] = new PerTrial(state)

  /** State rebuilt before every timed batch. */
  def perBatch[S](state: => S): Input[S] = new PerBatch(state)

  /** State rebuilt before every invocation. */
  def perInvocation[S](state: => S): Input[S] = new PerInvocation(state)

  /** No state, for bodies that need none. */
  val empty: Input[Unit] = perTrial(())

  private[sbench] final class PerTrial[S](state: => S) extends Input[S](state)

  private[sbench] final class PerBatch[S](state: => S) extends Input[S](state)

  private[sbench] final class PerInvocation[S](state: => S) extends Input[S](state)
}
