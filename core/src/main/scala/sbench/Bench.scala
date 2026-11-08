package sbench

object Bench {

  /** Configuration of a bench run. */
  final case class Options(
      runs: Int = Defaults.runs,
      warmup: Int = Defaults.warmup,
      batch: Int = Defaults.batch,
  )

  /** Result of a bench, with all timings per single invocation. */
  final case class Result(runs: Int, batch: Int, meanNs: Double, minNs: Double, maxNs: Double)

  /** Bench an input chain, whose `yield` body is the benchmark. */
  def bench[L <: Input.Lvl, A](chain: Input[L, A], opts: Options): Result = {
    require(opts.runs > 0, "runs must be positive")
    require(opts.warmup >= 0, "warmup must be non-negative")
    require(opts.batch > 0, "batch must be positive")

    val trial = Input.advance(chain.node, Input.trialStage)
    try {
      var w = 0
      while (w < opts.warmup) {
        timeBatch(trial.node, opts.batch)
        w += 1
      }

      var sum = 0.0
      var min = Double.PositiveInfinity
      var max = Double.NegativeInfinity
      var r = 0
      while (r < opts.runs) {
        val perOp = timeBatch(trial.node, opts.batch).toDouble / opts.batch
        sum += perOp
        if (perOp < min) min = perOp
        if (perOp > max) max = perOp
        r += 1
      }

      Result(opts.runs, opts.batch, sum / opts.runs, min, max)
    } finally Input.releaseAll(trial.releases)
  }

  private def timeBatch(trial: Input.Node, batch: Int): Long = {
    val perBatch = Input.advance(trial, Input.batchStage)
    try timeInvocations(perBatch.node, batch)
    finally Input.releaseAll(perBatch.releases)
  }

  private def timeInvocations(node: Input.Node, batch: Int): Long =
    node match {
      // No per-invocation inputs: the body is the only thing left, so call it
      // straight.
      case Input.Mapped(Input.Pure(state), body) =>
        val t0 = System.nanoTime()
        var i = 0
        while (i < batch) {
          Blackhole.consume(body(state))
          i += 1
        }
        System.nanoTime() - t0

      // Per-invocation inputs are built by `invocation()` itself, so they are
      // measured along with the body.
      case node =>
        val invocation = Input.compile(node)
        val t0 = System.nanoTime()
        var i = 0
        while (i < batch) {
          Blackhole.consume(invocation())
          i += 1
        }
        System.nanoTime() - t0
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
