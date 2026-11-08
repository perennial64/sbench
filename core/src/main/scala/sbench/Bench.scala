package sbench
import sbench.Input.PerTrial
import sbench.Input.PerBatch
import sbench.Input.PerInvocation

object Bench {

  /** Configuration of a bench run. */
  final case class Options(
      runs: Int = Defaults.runs,
      warmup: Int = Defaults.warmup,
      batch: Int = Defaults.batch,
  )

  /** Result of a bench, with all timings per single invocation. */
  final case class Result(runs: Int, batch: Int, meanNs: Double, minNs: Double, maxNs: Double)

  /** Bench given function with specified options. */
  def bench[S](input: Input[S], opts: Options)(body: S => Any): Result = {
    require(opts.runs > 0, "runs must be positive")
    require(opts.warmup >= 0, "warmup must be non-negative")
    require(opts.batch > 0, "batch must be positive")

    val batch = runBatch(input, opts.batch, body)

    var w = 0
    while (w < opts.warmup) {
      batch()
      w += 1
    }

    var sum = 0.0
    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity
    var r = 0
    while (r < opts.runs) {
      val perOp = batch().toDouble / opts.batch
      sum += perOp
      if (perOp < min) min = perOp
      if (perOp > max) max = perOp
      r += 1
    }

    Result(opts.runs, opts.batch, sum / opts.runs, min, max)
  }

  private def runBatch[S](input: Input[S], batch: Int, body: S => Any): () => Long = input match {
    case _: PerTrial[_] =>
      val s = input.get
      () => {
        val t0 = System.nanoTime()
        var i = 0
        while (i < batch) {
          Blackhole.consume(body(s))
          i += 1
        }
        System.nanoTime() - t0
      }

    case _: PerBatch[_] =>
      () => {
        val s = input.get
        val t0 = System.nanoTime()
        var i = 0
        while (i < batch) { Blackhole.consume(body(s)); i += 1 }
        System.nanoTime() - t0
      }

    case _: PerInvocation[_] =>
      val states = new Array[Any](batch)
      () => {
        var i = 0
        while (i < batch) {
          states(i) = input.get
          i += 1
        }

        val t0 = System.nanoTime()
        i = 0

        while (i < batch) {
          Blackhole.consume(body(states(i).asInstanceOf[S]))
          i += 1
        }

        System.nanoTime() - t0
      }
  }

  object Defaults {

    /** Default amount of benchmark run repetitions. */
    val runs: Int = 5

    /** Default amount of warmup calls for this platform. */
    val warmup: Int = 3

    /** Default amount of invocations timed as one batch. */
    val batch: Int = 1
  }
}
