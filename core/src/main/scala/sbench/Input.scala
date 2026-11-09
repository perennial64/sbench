package sbench

/** Input state producer for a benchmark body.
  *
  * Inputs declare (possibly chained) factories of data to be consumed by
  * benchmark body, specifying when it should be rebuilt in benching cycle.
  *
  * Inputs are typically introduced in a chained `for` comprehension, coarsest
  * first, with its `yield` body being the benchmark.
  */
final class Input[L <: Input.Lvl, A] private (private[Input] val node: Input.Node) {

  /** Introduce a further input, which must be no coarser than this one. The
    * result keeps this input's level, which stays the coarsest in the chain.
    */
  def flatMap[L2 <: L, B](f: A => Input[L2, B]): Input[L, B] =
    new Input(Input.Bind(node, value => f(value.asInstanceOf[A]).node))

  /** The benchmark body, to be run once per invocation, inside the timed
    * region.
    */
  def map[B](f: A => B): Input[L, B] =
    new Input(Input.Mapped(node, value => f(value.asInstanceOf[A])))

  /** Check this value once, outside of any measurement, failing the benchmark
    * with `label` when `check` does not hold.
    *
    * Use it to make sure a bench measures what it means to: a body whose result
    * is never checked could be optimized away and lead to unintended
    * measurements.
    */
  def verify(label: String)(check: A => Boolean): Input[L, A] =
    new Input(Input.Verified(node, value => check(value.asInstanceOf[A]), label))
}

object Input {

  /** When an input is rebuilt. */
  sealed trait Lvl

  /** Built once per benchmark. */
  sealed trait Trial extends Lvl

  /** Built once per thread a bench runs on. */
  sealed trait Thread extends Trial

  /** Rebuilt before every timed batch. */
  sealed trait Batch extends Thread

  /** Rebuilt before every invocation, inside the timed region. */
  sealed trait Invocation extends Batch

  /** State rebuilt once per benchmark, outside of the timed region. Correct
    * when the body does not mutate it.
    *
    * @param state
    *   Producer of the state
    * @param release
    *   Optional release of the state, runs after the last run
    */
  def perTrial[A](state: => A, release: A => Unit = noRelease): Input[Trial, A] =
    new Input(leaf(trialStage, () => state, release))

  /** State built once per thread a bench runs on, outside of the timed region,
    * and private to that thread.
    *
    * Where [[perTrial]] state is shared by every thread, this is not, so it
    * suits state a body mutates but that is too costly to rebuild per batch.
    * With a single thread the two differ only in that this one is built after
    * it.
    *
    * @param state
    *   Producer of the state
    * @param release
    *   Optional release of the state, runs after that thread's last batch
    */
  def perThread[A](state: => A, release: A => Unit = noRelease): Input[Thread, A] =
    new Input(leaf(threadStage, () => state, release))

  /** State rebuilt before every timed batch, outside of the timed region.
    * Correct for bodies that mutate their state, with each batch starting over
    * from the same point.
    *
    * @param state
    *   Producer of the state
    * @param release
    *   Optional release of the state, runs after each batch
    */
  def perBatch[A](state: => A, release: A => Unit = noRelease): Input[Batch, A] =
    new Input(leaf(batchStage, () => state, release))

  /** State rebuilt before every invocation, which is measured along with the
    * body. Unlike the coarser levels, a throwing body skips the release.
    *
    * Reach for this level only when building the state is cheap next to the
    * body, or when the body cannot be benched any other way.
    *
    * Inconveniences of this level comes from the fact that building it cannot
    * be excluded from the measurement: the clock is read once per batch, while
    * this state is rebuilt once per invocation. Reading the clock around each
    * invocation instead would cost more than many bodies do, and would perturb
    * the very loop it is meant to measure.
    *
    * @param state
    *   Producer of the state
    * @param release
    *   Optional release of the state
    */
  def perInvocation[A](state: => A, release: A => Unit = noRelease): Input[Invocation, A] =
    new Input(leaf(invocationStage, () => state, release))

  /** Blackhole sink, one per thread a bench runs on. */
  def blackhole: Input[Thread, Blackhole] = new Input(Sink)

  /** No state. */
  val empty: Input[Trial, Unit] = perTrial(())

  /** Release nothing. */
  private val noRelease: Any => Unit = _ => ()

  /** Thrown when a [[Input.verify]] check does not hold. */
  final class VerificationFailed(val label: String) extends RuntimeException("benchmark verification failed: " + label)

  private def leaf[A](stage: Int, make: () => A, release: A => Unit): Node =
    Leaf(stage, make.asInstanceOf[() => Any], value => release(value.asInstanceOf[A]))

  /** A chain built up to some stage, with the releases owed for it so far. */
  private[sbench] final class Staged private[Input] (
      private[Input] val node: Node,
      private[Input] val releases: List[() => Unit],
  )

  /** How to run one invocation of a staged chain. */
  private[sbench] sealed trait Runner

  private[sbench] object Runner {

    /** Nothing is left but the body, so it can be called straight. */
    final case class Direct(state: Any, body: Any => Any) extends Runner

    /** Per-invocation inputs are left, so they are built by `run` itself and
      * are measured along with the body.
      */
    final case class Thunk(run: () => Any) extends Runner
  }

  /** Build the per-trial inputs, once per benchmark. */
  private[sbench] def stageTrial(chain: Input[_, _], sink: Blackhole): Staged =
    advance(chain.node, trialStage, sink)

  /** Build the per-thread inputs, once per thread, outside of the timed region.
    */
  private[sbench] def stageThread(trial: Staged, sink: Blackhole): Staged =
    advance(trial.node, threadStage, sink)

  /** Build the per-batch inputs, once per batch, outside of the timed region.
    */
  private[sbench] def stageBatch(thread: Staged, sink: Blackhole): Staged =
    advance(thread.node, batchStage, sink)

  /** Run a staged chain once, untimed, applying the checks that are left. */
  private[sbench] def verifyOnce(staged: Staged, sink: Blackhole): Unit = {
    var releases: List[() => Unit] = Nil

    def eval(current: Node): Any = current match {
      case Pure(value) => value
      case Sink => sink
      case Leaf(_, make, release) =>
        val value = make()
        releases = (() => release(value)) :: releases
        value
      case Bind(src, f) => eval(f(eval(src)))
      case Mapped(src, f) => f(eval(src))
      case Verified(src, check, label) =>
        val value = eval(src)
        if (!check(value)) throw new VerificationFailed(label)
        value
    }

    try {
      val _ = eval(staged.node)
    } finally releases.foreach(_())
  }

  /** Reduce a staged chain to one invocation's worth of work. */
  private[sbench] def runner(staged: Staged): Runner = strip(staged.node) match {
    case Mapped(Pure(state), body) => Runner.Direct(state, body)
    case other => Runner.Thunk(compile(other))
  }

  /** Release what a stage built, in the order it was reported. */
  private[sbench] def release(staged: Staged): Unit = staged.releases.foreach(_())

  private val trialStage = 0
  private val threadStage = 1
  private val batchStage = 2
  private val invocationStage = 3

  private[sbench] sealed trait Node
  private final case class Pure(value: Any) extends Node
  private final case class Leaf(stage: Int, make: () => Any, release: Any => Unit) extends Node
  private final case class Bind(src: Node, f: Any => Node) extends Node
  private final case class Mapped(src: Node, f: Any => Any) extends Node
  private final case class Verified(src: Node, check: Any => Boolean, label: String) extends Node
  private case object Sink extends Node

  /** Build every input of `stage` or coarser, leaving finer ones suspended. */
  private def advance(node: Node, stage: Int, sink: Blackhole): Staged = node match {
    case Pure(_) => new Staged(node, Nil)

    case Sink =>
      if (threadStage <= stage) new Staged(Pure(sink), Nil) else new Staged(node, Nil)

    case Leaf(leafStage, make, release) =>
      if (leafStage > stage) new Staged(node, Nil)
      else {
        val value = make()
        new Staged(Pure(value), List(() => release(value)))
      }

    case Bind(src, f) =>
      val staged = advance(src, stage, sink)
      staged.node match {
        case Pure(value) =>
          val rest = advance(f(value), stage, sink)
          // Whatever was built last is released first
          new Staged(rest.node, rest.releases ::: staged.releases)
        case suspended => new Staged(Bind(suspended, f), staged.releases)
      }

    case Mapped(src, f) =>
      val staged = advance(src, stage, sink)
      new Staged(Mapped(staged.node, f), staged.releases)

    case Verified(src, check, label) =>
      val staged = advance(src, stage, sink)
      staged.node match {
        case Pure(value) =>
          if (!check(value)) throw new VerificationFailed(label)
          staged
        case suspended => new Staged(Verified(suspended, check, label), staged.releases)
      }
  }

  /** Flatten what is left of a chain into a single function. */
  private def compile(node: Node): () => Any = node match {
    case Pure(value) => () => value
    case Verified(src, _, _) => compile(src)
    case Mapped(Pure(value), f) => () => f(value)

    case Leaf(_, make, release) =>
      () => {
        val value = make()
        release(value)
        value
      }

    case Mapped(Leaf(_, make, release), f) =>
      () => {
        val value = make()
        val out = f(value)
        release(value)
        out
      }

    case Bind(Leaf(_, make, release), f) =>
      () => {
        val value = make()
        val out = runOnce(f(value))
        release(value)
        out
      }

    case Mapped(src, f) =>
      val run = compile(src)
      () => f(run())

    case Bind(src, f) =>
      val run = compile(src)
      () => runOnce(f(run()))

    case Sink => throw new IllegalStateException("blackhole outlived its batch")
  }

  /** Run what is left of a chain: the body, and any per-invocation inputs. */
  private def runOnce(node: Node): Any = node match {
    case Pure(value) => value
    case Verified(src, _, _) => runOnce(src)

    case Leaf(_, make, release) =>
      val value = make()
      release(value)
      value

    case Mapped(Leaf(_, make, release), f) =>
      val value = make()
      val out = f(value)
      release(value)
      out

    case Bind(Leaf(_, make, release), f) =>
      val value = make()
      val out = runOnce(f(value))
      release(value)
      out

    case Mapped(src, f) => f(runOnce(src))
    case Bind(src, f) => runOnce(f(runOnce(src)))
    case Sink => throw new IllegalStateException("blackhole outlived its batch")
  }

  private def strip(node: Node): Node = node match {
    case Verified(src, _, _) => strip(src)
    case Mapped(src, f) => Mapped(strip(src), f)
    case Bind(src, f) => Bind(strip(src), f)
    case other => other
  }
}
