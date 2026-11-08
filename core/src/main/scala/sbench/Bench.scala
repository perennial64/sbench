package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

object Bench {

  /** Configuration of a bench run. */
  final case class Options(
      warmup: FiniteDuration = Defaults.warmup,
      samples: Int = Defaults.samples,
      batch: Batch = Defaults.batch,
      work: Work = Defaults.work,
  )

  /** How many invocations are timed as one sample. */
  sealed trait Batch

  object Batch {

    /** Ramp the count over the samples, so that the per-invocation cost can be
      * estimated by regression and fixed per-sample cost drops out of it.
      */
    final case class Ramped(sampleTime: FiniteDuration = Defaults.sampleTime) extends Batch {
      require(sampleTime.toNanos > 0, "sampleTime must be positive")
    }

    /** Size each sample to take about `sampleTime`, measured after warmup, and
      * keep that count for every sample.
      */
    final case class Timed(sampleTime: FiniteDuration = Defaults.sampleTime) extends Batch {
      require(sampleTime.toNanos > 0, "sampleTime must be positive")
    }

    /** Time exactly `size` invocations per sample. */
    final case class Fixed(size: Int) extends Batch {
      require(size > 0, "batch size must be positive")
    }
  }

  /** One timed sample: how many invocations ran, and how long they took. */
  final case class Sample(iterations: Long, elapsedNs: Long) {
    require(iterations > 0, "a sample needs at least one invocation")

    /** Time per invocation, with any fixed per-sample cost still in it. */
    def perOpNs: Double = elapsedNs.toDouble / iterations
  }

  /** Samples beyond Tukey's fences, classified as criterion classifies them. */
  final case class Outliers(lowSevere: Int, lowMild: Int, highMild: Int, highSevere: Int) {
    def mild: Int = lowMild + highMild
    def severe: Int = lowSevere + highSevere
    def total: Int = mild + severe
  }

  /** How much logical work one invocation does.
    *
    * Declaring it lets a report derive throughput.
    */
  sealed trait Work

  object Work {

    /** One operation per invocation, so there is no throughput to report. */
    case object One extends Work

    /** `count` logical operations per invocation. */
    final case class Ops(count: Long) extends Work {
      require(count > 0, "work count must be positive")
    }

    /** `count` bytes processed per invocation. */
    final case class Bytes(count: Long) extends Work {
      require(count > 0, "work count must be positive")
    }
  }

  /** Result of a bench: every sample it took, and the work each invocation did.
    */
  final case class Result(samples: Vector[Sample], work: Work = Work.One) {
    require(samples.nonEmpty, "a result needs at least one sample")

    /** How many samples were taken. */
    def sampleCount: Int = samples.length

    /** Per-invocation time of each sample. */
    def perOpNs: Vector[Double] = samples.map(_.perOpNs)

    private lazy val sorted: Vector[Double] = perOpNs.sorted

    /** Whether the samples ran different iteration counts. */
    def ramped: Boolean = sampleCount > 2 && samples.exists(_.iterations != samples.head.iterations)

    private lazy val fit: Option[Fit] = if (ramped) Fit.of(samples) else None

    /** The headline per-invocation estimate. */
    def estimateNs: Double = fit.map(_.slope).getOrElse(meanNs)

    /** Fixed cost per sample, when it could be estimated apart. */
    def overheadNs: Option[Double] = fit.map(_.intercept)

    /** How well a straight line described the samples, when one was fitted. */
    def rSquared: Option[Double] = fit.map(_.rSquared)

    /** Half width of a 95% confidence interval of [[estimateNs]]. */
    def errorNs: Double = fit match {
      case Some(f) => Stats.tValue(sampleCount - 2) * f.slopeError
      case None =>
        if (sampleCount < 2) 0.0
        else Stats.tValue(sampleCount - 1) * stdDevNs / math.sqrt(sampleCount.toDouble)
    }

    def lowNs: Double = estimateNs - errorNs
    def highNs: Double = estimateNs + errorNs

    def meanNs: Double = perOpNs.sum / sampleCount
    def minNs: Double = sorted.head
    def maxNs: Double = sorted.last
    def medianNs: Double = percentileNs(50)

    /** The `p`th percentile of the per-invocation times, by nearest rank. */
    def percentileNs(p: Double): Double = {
      val rank = math.ceil(p / 100.0 * sampleCount).toInt - 1
      sorted(math.min(sampleCount - 1, math.max(0, rank)))
    }

    /** Sample standard deviation, zero when there is nothing to vary. */
    def stdDevNs: Double =
      if (sampleCount < 2) 0.0
      else {
        val mean = meanNs
        math.sqrt(perOpNs.map(v => (v - mean) * (v - mean)).sum / (sampleCount - 1))
      }

    /** Standard deviation as a share of the mean, the handiest noise measure.
      */
    def relStdDevPct: Double = if (meanNs == 0.0) 0.0 else stdDevNs / meanNs * 100.0

    def outliers: Outliers = {
      val q1 = percentileNs(25)
      val q3 = percentileNs(75)
      val iqr = q3 - q1
      var lowSevere, lowMild, highMild, highSevere = 0
      perOpNs.foreach { v =>
        if (v < q1 - 3.0 * iqr) lowSevere += 1
        else if (v < q1 - 1.5 * iqr) lowMild += 1
        else if (v > q3 + 3.0 * iqr) highSevere += 1
        else if (v > q3 + 1.5 * iqr) highMild += 1
      }
      Outliers(lowSevere, lowMild, highMild, highSevere)
    }

    /** Invocations per second, whatever work each one does. */
    def invocationsPerSecond: Double = if (estimateNs == 0.0) 0.0 else 1e9 / estimateNs

    /** Work units per second at a given per-invocation time, or `None` when no
      * work was declared.
      */
    def throughputAt(ns: Double): Option[Double] = work match {
      case Work.One => None
      case _ if ns <= 0.0 => None
      case Work.Ops(count) => Some(count * 1e9 / ns)
      case Work.Bytes(count) => Some(count * 1e9 / ns)
    }

    /** Work units per second at [[estimateNs]]. */
    def throughput: Option[Double] = throughputAt(estimateNs)
  }

  /** A least squares fit of elapsed time against iteration count. */
  private[sbench] final case class Fit(
      slope: Double,
      intercept: Double,
      slopeError: Double,
      rSquared: Double,
  )

  private[sbench] object Fit {

    def of(samples: Vector[Sample]): Option[Fit] = {
      val n = samples.length
      if (n < 3) None
      else {
        val xs = samples.map(_.iterations.toDouble)
        val ys = samples.map(_.elapsedNs.toDouble)
        val meanX = xs.sum / n
        val meanY = ys.sum / n
        val sxx = xs.map(x => (x - meanX) * (x - meanX)).sum
        if (sxx <= 0.0) None
        else {
          val sxy = xs.zip(ys).map { case (x, y) => (x - meanX) * (y - meanY) }.sum
          val slope = sxy / sxx
          val intercept = meanY - slope * meanX
          val sse = xs
            .zip(ys)
            .map { case (x, y) =>
              val residual = y - (intercept + slope * x)
              residual * residual
            }
            .sum
          val sst = ys.map(y => (y - meanY) * (y - meanY)).sum
          Some(
            Fit(
              slope = slope,
              intercept = intercept,
              slopeError = math.sqrt(sse / (n - 2) / sxx),
              rSquared = if (sst <= 0.0) 1.0 else 1.0 - sse / sst,
            )
          )
        }
      }
    }
  }

  private[sbench] object Stats {

    /** Two sided 95% critical values of Student's t, by degrees of freedom. */
    private val t95: Array[Double] = Array(12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 2.262, 2.228, 2.201,
      2.179, 2.160, 2.145, 2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 2.060, 2.056, 2.052,
      2.048, 2.045, 2.042)

    def tValue(df: Int): Double =
      if (df <= 0) 0.0
      else if (df <= t95.length) t95(df - 1)
      else if (df <= 40) 2.021
      else if (df <= 60) 2.000
      else if (df <= 120) 1.980
      else 1.960
  }

  /** Bench an input chain, whose `yield` body is the benchmark. */
  def bench[L <: Input.Lvl, A](chain: Input[L, A], opts: Options): Result = {
    require(opts.samples > 0, "samples must be positive")
    require(opts.warmup.toNanos >= 0, "warmup must be non-negative")

    val trial = Input.advance(chain.node, Input.trialStage)
    try {
      // One untimed verification invocation up front
      Input.verifyOnce(trial.node)

      // Calibrate twice
      val cold = perOpEstimate(trial.node)
      warmUp(trial.node, math.max(1L, (1000000.0 / cold).toLong).toInt, opts.warmup.toNanos)
      val perOp = perOpEstimate(trial.node)

      val plan = samplePlan(opts, perOp)
      val samples = Vector.newBuilder[Sample]
      var s = 0
      while (s < plan.length) {
        val iterations = plan(s)
        samples += Sample(iterations.toLong, timeBatch(trial.node, iterations))
        s += 1
      }

      Result(samples.result(), opts.work)
    } finally Input.releaseAll(trial.releases)
  }

  /** How many invocations each sample should run. */
  private def samplePlan(opts: Options, perOpNs: Double): Vector[Int] = opts.batch match {
    case Batch.Fixed(size) => Vector.fill(opts.samples)(size)

    case Batch.Timed(sampleTime) =>
      Vector.fill(opts.samples)(sized(sampleTime.toNanos.toDouble, perOpNs))

    case Batch.Ramped(sampleTime) =>
      val n = opts.samples
      val triangle = n.toDouble * (n + 1) / 2
      val budget = n.toDouble * sampleTime.toNanos
      val step = budget / (perOpNs * triangle)
      if (step < 1.0) Vector.fill(n)(sized(sampleTime.toNanos.toDouble, perOpNs))
      else shuffle(Vector.tabulate(n)(i => clamp((i + 1) * step)))
  }

  /** Reorder a ramp so that sample size is not tied to when it ran. */
  private def shuffle(plan: Vector[Int]): Vector[Int] = {
    val n = plan.length
    val stride = Defaults.shuffleStride
    if (n < 3 || n % stride == 0) plan
    else Vector.tabulate(n)(i => plan((i.toLong * stride % n).toInt))
  }

  private def sized(targetNs: Double, perOpNs: Double): Int = clamp(targetNs / perOpNs)

  private def clamp(count: Double): Int =
    math.max(1.0, math.min(math.ceil(count), Defaults.maxBatch.toDouble)).toInt

  /** Run batches until `warmupNs` has elapsed. */
  private def warmUp(trial: Input.Node, batch: Int, warmupNs: Long): Unit = {
    val deadline = System.nanoTime() + warmupNs
    while (System.nanoTime() < deadline) {
      timeBatch(trial, batch)
      ()
    }
  }

  /** Estimate the per-invocation cost, growing the batch until a sample lasts
    * long enough to extrapolate from, well clear of the clock's granularity.
    */
  private def perOpEstimate(trial: Input.Node): Double = {
    var batch = 1
    var elapsed = timeBatch(trial, batch)
    while (elapsed < Defaults.calibrationNs && batch <= Defaults.maxBatch / 8) {
      batch *= 8
      elapsed = timeBatch(trial, batch)
    }
    math.max(elapsed.toDouble / batch, 1e-3)
  }

  private def timeBatch(trial: Input.Node, batch: Int): Long = {
    val perBatch = Input.advance(trial, Input.batchStage)
    try timeInvocations(Input.strip(perBatch.node), batch)
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
      case other =>
        val invocation = Input.compile(other)
        val t0 = System.nanoTime()
        var i = 0
        while (i < batch) {
          Blackhole.consume(invocation())
          i += 1
        }
        System.nanoTime() - t0
    }

  object Defaults {

    /** Time spent warming up before any sample is kept, per platform. */
    val warmup: FiniteDuration = Platform.warmup

    /** Target duration of a single sample, when a batch is sized by time. */
    val sampleTime: FiniteDuration = 25.millis

    /** How many samples to keep. Enough that Student's t is not punishing. */
    val samples: Int = 30

    /** Report no throughput unless a bench declares its work. */
    val work: Work = Work.One

    /** Let the harness size batches by default. */
    val batch: Batch = Batch.Timed()

    /** A calibration sample has to last this long to be worth extrapolating
      * from.
      */
    private[sbench] val calibrationNs: Long = 1000000L

    private[sbench] val maxBatch: Int = 1 << 28

    private[sbench] val shuffleStride: Int = 7
  }
}
