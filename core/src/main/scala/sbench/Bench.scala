package sbench

object Bench {

  /** Configuration of a bench run. */
  final case class Options(runs: Int, warmup: Int)

  /** Result of a bench. */
  final case class Result(runs: Int, meanNs: Double, minNs: Long, maxNs: Long)

  /** Bench given function with specified options. */
  def bench(opts: Options)(body: => Unit): Result = {
    require(opts.runs > 0, "runs must be positive")
    require(opts.warmup >= 0, "warmup must be non-negative")

    var i = 0
    while (i < opts.warmup) {
      body
      i += 1
    }

    var totalNs = 0L
    var minNs = Long.MaxValue
    var maxNs = Long.MinValue
    i = 0
    while (i < opts.runs) {
      val start = System.nanoTime()
      body
      val elapsedNs = System.nanoTime() - start
      totalNs += elapsedNs
      if (elapsedNs < minNs) minNs = elapsedNs
      if (elapsedNs > maxNs) maxNs = elapsedNs
      i += 1
    }

    Result(opts.runs, totalNs.toDouble / opts.runs, minNs, maxNs)
  }
}
