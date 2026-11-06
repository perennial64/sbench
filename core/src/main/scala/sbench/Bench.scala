package sbench

object Bench {

  /** Configuration of a bench run. */
  final case class Options(runs: Int = Defaults.runs, warmup: Int = Defaults.warmup)

  /** Result of a bench. */
  final case class Result(runs: Int, meanNs: Double, minNs: Long, maxNs: Long)

  /** Bench given function with specified options. */
  def bench[S](input: Input[S], opts: Options)(body: S => Any): Result = {
    require(opts.runs > 0, "runs must be positive")
    require(opts.warmup >= 0, "warmup must be non-negative")

    var i = 0
    while (i < opts.warmup) {
      val state = input.get
      Blackhole.consume(body(state))
      i += 1
    }

    var totalNs = 0L
    var minNs = Long.MaxValue
    var maxNs = Long.MinValue
    i = 0
    while (i < opts.runs) {
      val state = input.get
      val start = System.nanoTime()
      Blackhole.consume(body(state))
      val elapsedNs = System.nanoTime() - start
      totalNs += elapsedNs
      if (elapsedNs < minNs) minNs = elapsedNs
      if (elapsedNs > maxNs) maxNs = elapsedNs
      i += 1
    }

    Result(opts.runs, totalNs.toDouble / opts.runs, minNs, maxNs)
  }

  object Defaults {

    /** Default amount of benchmark run repetitions. */
    val runs: Int = 5

    /** Default amount of warmup calls for this platform. */
    val warmup: Int = 3
  }
}
