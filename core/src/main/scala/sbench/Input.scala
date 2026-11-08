package sbench

import scala.annotation.implicitNotFound

/** Typed state for a benchmark body.
  *
  * Inputs are introduced in a `for` comprehension, declaring when it is
  * rebuilt, with its `yield` body being the timed benchmark.
  *
  * Inputs have to be introduced coarsest first.
  */
final class Input[L <: Input.Lvl, A] private[sbench] (private[sbench] val node: Input.Node) {

  /** Introduce a further input, which must be no coarser than this one. */
  def flatMap[L2 <: Input.Lvl, B](f: A => Input[L2, B])(implicit order: Input.Leq[L, L2]): Input[L, B] = {
    val _ = order
    new Input(Input.Bind(node, a => f(a.asInstanceOf[A]).node))
  }

  /** The benchmark body. */
  def map[B](f: A => B): Input[L, B] =
    new Input(Input.Mapped(node, a => f(a.asInstanceOf[A])))
}

object Input {

  /** When an input is rebuilt. */
  sealed trait Lvl

  /** Built once per benchmark. */
  sealed trait Trial extends Lvl

  /** Rebuilt before every timed batch. */
  sealed trait Batch extends Lvl

  /** Rebuilt before every invocation, inside the timed region. */
  sealed trait Invocation extends Lvl

  /** Evidence that an `L1` input may be followed by an `L2`. */
  @implicitNotFound("inputs must be introduced coarsest first: cannot introduce a ${L2} input after an ${L1}")
  sealed trait Leq[L1 <: Lvl, L2 <: Lvl]

  object Leq {
    private val instance = new Leq[Lvl, Lvl] {}
    private def unsafe[L1 <: Lvl, L2 <: Lvl]: Leq[L1, L2] = instance.asInstanceOf[Leq[L1, L2]]

    implicit val trialThenTrial: Leq[Trial, Trial] = unsafe
    implicit val trialThenBatch: Leq[Trial, Batch] = unsafe
    implicit val trialThenInvocation: Leq[Trial, Invocation] = unsafe
    implicit val batchThenBatch: Leq[Batch, Batch] = unsafe
    implicit val batchThenInvocation: Leq[Batch, Invocation] = unsafe
    implicit val invocationThenInvocation: Leq[Invocation, Invocation] = unsafe
  }

  /** State built once per benchmark. Correct when the body does not mutate it.
    */
  def perTrial[A](state: => A): Input[Trial, A] = new Input(Leaf(trialStage, () => state))

  /** State rebuilt before every timed batch, outside of the timed region. Use
    * for bodies that mutate their state, so that each batch starts over from
    * the same point.
    */
  def perBatch[A](state: => A): Input[Batch, A] = new Input(Leaf(batchStage, () => state))

  /** State rebuilt before every invocation, which is measured along with the
    * body.
    *
    * Reach for this level only when building the state is cheap next to the
    * body, or when the body cannot be benched any other way.
    */
  def perInvocation[A](state: => A): Input[Invocation, A] = new Input(Leaf(invocationStage, () => state))

  /** No state. */
  val empty: Input[Trial, Unit] = perTrial(())

  private[sbench] val trialStage = 0
  private[sbench] val batchStage = 1
  private[sbench] val invocationStage = 2

  private[sbench] sealed trait Node
  private[sbench] final case class Pure(value: Any) extends Node
  private[sbench] final case class Leaf(stage: Int, make: () => Any) extends Node
  private[sbench] final case class Bind(src: Node, f: Any => Node) extends Node
  private[sbench] final case class Mapped(src: Node, f: Any => Any) extends Node

  /** Evaluate every input of `stage` or coarser, leaving finer ones suspended.
    */
  private[sbench] def advance(node: Node, stage: Int): Node = node match {
    case Pure(_) => node
    case Leaf(leafStage, make) => if (leafStage <= stage) Pure(make()) else node
    case Bind(src, f) =>
      advance(src, stage) match {
        case Pure(value) => advance(f(value), stage)
        case suspended => Bind(suspended, f)
      }
    case Mapped(src, f) => Mapped(advance(src, stage), f)
  }

  /** Flatten whatever is left of a chain into a single function. */
  private[sbench] def compile(node: Node): () => Any = node match {
    case Pure(value) => () => value
    case Leaf(_, make) => make
    case Mapped(Pure(value), f) => () => f(value)
    case Mapped(src, f) =>
      val run = compile(src)
      () => f(run())
    case Bind(src, f) =>
      val run = compile(src)
      () => runOnce(f(run()))
  }

  /** Run whatever is left of a chain, which is the body and any per-invocation
    * inputs.
    */
  private[sbench] def runOnce(node: Node): Any = node match {
    case Pure(value) => value
    case Leaf(_, make) => make()
    case Mapped(src, f) => f(runOnce(src))
    case Bind(src, f) => runOnce(f(runOnce(src)))
  }
}
