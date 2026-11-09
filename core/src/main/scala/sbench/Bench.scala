package sbench

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration

object Bench {

  /** Configuration of a bench run.
    *
    * @param warmup
    *   Time for which bench body would rerun until being considered warmed up
    *   (JIT, branch predictors) and timed
    * @param samples
    *   How many timed samples to keep, which the estimate and its interval are
    *   drawn from
    * @param threads
    *   How many threads run the body at once, for bodies whose behaviour under
    *   contention is the point
    * @param batch
    *   Which batching strategy to use
    * @param work
    *   How to measure throughput, if desired
    */
  final case class Options(
      warmup: FiniteDuration = Defaults.warmup,
      samples: Int = Defaults.samples,
      threads: Int = Defaults.threads,
      batch: Batch = Defaults.batch,
      work: Work = Defaults.work,
  ) {
    require(samples > 0, "samples must be positive")
    require(warmup.toNanos >= 0, "warmup must be non-negative")
    require(threads >= 1, "threads must be positive")
    require(
      threads == 1 || Platform.supportsThreads,
      if (Platform.cores > 1)
        "this build runs a single thread: enable multithreading in nativeConfig to use more"
      else "this platform runs a single thread",
    )
  }

  /** How many invocations are timed as one [[Sample]]. */
  sealed trait Batch

  object Batch {

    /** Size each sample to take about `sampleTime`, measured after warmup, and
      * keep that count for every sample.
      *
      * Correct for bodies that complete too fast to be measured separately, so
      * that the runtime would choose a reasonable batching amount.
      */
    final case class Timed(sampleTime: FiniteDuration = Defaults.sampleTime) extends Batch {
      require(sampleTime.toNanos > 0, "sampleTime must be positive")
    }

    /** Ramp the count across the samples, spending `samples * sampleTime` in
      * total, so that the per-invocation cost can be fitted as a slope instead
      * of carrying each sample's fixed cost.
      *
      * Correct for bodies whose invocations cost the same throughout.
      */
    final case class Ramped(sampleTime: FiniteDuration = Defaults.sampleTime) extends Batch {
      require(sampleTime.toNanos > 0, "sampleTime must be positive")
    }

    /** Time exactly `size` invocations per sample.
      *
      * Correct for bodies whose invocations grow more expensive as they go:
      * pinned to a count, every sample covers the same stretch of that growth
      * from the same starting point. Paired with [[Input.perBatch]] state,
      * result can be read as the mean over the first `size` invocations rather
      * than as a cost of one.
      */
    final case class Fixed(size: Int) extends Batch {
      require(size > 0, "batch size must be positive")
    }
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

  /** One timed sample, of any number of invocations, measuring the total time
    * they took.
    */
  final case class Sample(iterations: Long, elapsedNs: Long) {
    require(iterations > 0, "a sample needs at least one invocation")

    /** Time one invocation took. */
    def perOpNs: Double = elapsedNs.toDouble / iterations
  }

  /** Result of a bench: every sample it took, and the work each invocation did.
    */
  final case class Result(samples: Vector[Sample], threads: Int = 1, work: Work = Work.One) {
    require(samples.nonEmpty, "a result needs at least one sample")

    private lazy val sorted: Vector[Double] = perOpNs.sorted

    private lazy val fit: Option[Fit] = if (ramped) Fit.of(samples) else None

    /** How many samples were taken. */
    def sampleCount: Int = samples.length

    /** Per-invocation time of each sample. */
    def perOpNs: Vector[Double] = samples.map(_.perOpNs)

    /** Whether the samples ran different iteration counts, so that
      * [[estimateNs]] is a fitted slope and [[overheadNs]] and [[rSquared]] are
      * known.
      */
    def ramped: Boolean =
      sampleCount > 2 && samples.exists(_.iterations != samples.head.iterations)

    /** The headline per-invocation estimate: the fitted slope when the samples
      * ramped, their mean otherwise.
      */
    def estimateNs: Double = fit.fold(meanNs)(_.slope)

    /** Fixed cost per sample, when it could be estimated apart. */
    def overheadNs: Option[Double] = fit.map(_.intercept)

    /** How well a straight line described the samples, when one was fitted. */
    def rSquared: Option[Double] = fit.map(_.rSquared)

    /** Half width of a 95% confidence interval of [[estimateNs]]. */
    def errorNs: Double = fit match {
      case Some(fitted) => tValue(sampleCount - 2) * fitted.slopeError
      case None =>
        if (sampleCount < 2) 0.0
        else tValue(sampleCount - 1) * stdDevNs / math.sqrt(sampleCount.toDouble)
    }

    /** Low end of the confidence interval of [[estimateNs]]. */
    def lowNs: Double = estimateNs - errorNs

    /** High end of the confidence interval of [[estimateNs]]. */
    def highNs: Double = estimateNs + errorNs

    /** Mean of the per-invocation times, which [[estimateNs]] only matches when
      * the samples did not ramp.
      */
    def meanNs: Double = perOpNs.sum / sampleCount

    /** Fastest sample. */
    def minNs: Double = sorted.head

    /** Slowest sample. */
    def maxNs: Double = sorted.last

    /** Middle sample, i.e. [[percentileNs]] at 50. */
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

    /** Standard deviation as a share of [[meanNs]]. */
    def relStdDevPct: Double = if (meanNs == 0.0) 0.0 else stdDevNs / meanNs * 100.0

    /** Outliers classified by Tukey's boxplot rule. */
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

    /** Invocations per second across every thread, whatever work each one does.
      */
    def invocationsPerSecond: Double =
      if (estimateNs == 0.0) 0.0 else threads * 1e9 / estimateNs

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

  /** Classified samples beyond Tukey's fences. */
  final case class Outliers(lowSevere: Int, lowMild: Int, highMild: Int, highSevere: Int) {
    def mild: Int = lowMild + highMild
    def severe: Int = lowSevere + highSevere
    def total: Int = mild + severe
  }

  /** Hardware threads available on this platform/machine. */
  val cores: Int = if (Platform.supportsThreads) Platform.cores else 1

  /** Whether more than one thread can run on this platform. */
  val supportsThreads: Boolean = Platform.supportsThreads

  /** Bench an input chain with specified options. */
  def bench[L <: Input.Lvl, A](chain: Input[L, A], opts: Options = Options()): Result = {
    val sink = new Blackhole()
    val trial = Input.stageTrial(chain, sink)
    try {
      val plan = prepare(trial, sink, opts)
      val samples =
        if (opts.threads == 1) sampleOn(trial, sink, plan, opts.warmup.toNanos, warmed = true)
        else {
          // Every thread warms up under the contention it will be measured in.
          val collected = new Array[Vector[Sample]](opts.threads)
          Platform.runWorkers(opts.threads) { worker =>
            collected(worker) = sampleOn(trial, new Blackhole(), plan, opts.warmup.toNanos, warmed = false)
          }
          collected.toVector.flatten
        }

      Result(samples, opts.threads, opts.work)
    } finally Input.release(trial)
  }

  /** Verify the chain, warm it up and size its samples, on the calling thread.
    */
  private def prepare(trial: Input.Staged, sink: Blackhole, opts: Options): Vector[Int] = {
    val thread = Input.stageThread(trial, sink)
    try {
      // One untimed verification invocation up front
      Input.verifyOnce(thread, sink)

      opts.batch match {
        // A pinned count needs no estimate
        case Batch.Fixed(size) =>
          warmUp(thread, sink, size, opts.warmup.toNanos)
          Vector.fill(opts.samples)(size)

        // Sizing a sample by time has to estimate the cost of one invocation
        case _ =>
          if (opts.warmup.toNanos > 0)
            warmUp(
              thread,
              sink,
              sized(calibrationNs.toDouble, perOpEstimate(thread, sink)),
              opts.warmup.toNanos,
            )
          // Estimate again: the first one is cold, and on a tiered runtime the
          // warm one can be an order of magnitude apart from it.
          samplePlan(opts, perOpEstimate(thread, sink))
      }
    } finally Input.release(thread)
  }

  /** Take every sample of `plan` on this thread, warming up first unless the
    * caller already has.
    */
  private def sampleOn(
      trial: Input.Staged,
      sink: Blackhole,
      plan: Vector[Int],
      warmupNs: Long,
      warmed: Boolean,
  ): Vector[Sample] = {
    val thread = Input.stageThread(trial, sink)
    try {
      // Warm with the count the samples use, so a worker neither calibrates
      // nor warms on a batch size it will never run.
      if (!warmed) warmUp(thread, sink, plan.head, warmupNs)

      val samples = Vector.newBuilder[Sample]
      var i = 0
      while (i < plan.length) {
        val iterations = plan(i)
        samples += Sample(iterations.toLong, timeBatch(thread, sink, iterations))
        i += 1
      }
      samples.result()
    } finally Input.release(thread)
  }

  /** How many invocations each sample should run. */
  private def samplePlan(opts: Options, perOpNs: Double): Vector[Int] = opts.batch match {
    case Batch.Fixed(size) => Vector.fill(opts.samples)(size)

    case Batch.Timed(sampleTime) =>
      Vector.fill(opts.samples)(sized(sampleTime.toNanos.toDouble, perOpNs))

    case Batch.Ramped(sampleTime) =>
      val samples = opts.samples
      val triangle = samples.toDouble * (samples + 1) / 2
      val step = samples.toDouble * sampleTime.toNanos / (perOpNs * triangle)
      // One invocation per step already overruns the budget, so there is no
      // room to ramp: keep every sample the same size instead.
      if (step < 1.0) Vector.fill(samples)(sized(sampleTime.toNanos.toDouble, perOpNs))
      else spread(Vector.tabulate(samples)(i => clamp((i + 1) * step)))
  }

  /** Reorder a ramp so that a sample's size is not tied to when it ran.
    *
    * Without this every later sample is both larger and later, so drift over
    * the measurement window lands on the fitted slope as bias rather than as
    * noise. Steps by a coprime stride, needing no randomness and so staying
    * reproducible.
    */
  private def spread(plan: Vector[Int]): Vector[Int] = {
    val stride = 7
    if (plan.length < 3 || plan.length % stride == 0) plan
    else Vector.tabulate(plan.length)(i => plan((i.toLong * stride % plan.length).toInt))
  }

  /** Run batches of `count` invocations until `warmupNs` has elapsed. */
  private def warmUp(trial: Input.Staged, sink: Blackhole, count: Int, warmupNs: Long): Unit = {
    val deadline = System.nanoTime() + warmupNs
    while (System.nanoTime() < deadline) {
      timeBatch(trial, sink, count)
      ()
    }
  }

  /** Estimate the per-invocation cost, growing the count until a sample lasts
    * long enough to extrapolate from.
    */
  private def perOpEstimate(trial: Input.Staged, sink: Blackhole): Double = {
    var count = 1
    var elapsed = timeBatch(trial, sink, count)
    while (elapsed < calibrationNs && count <= maxBatch / 8) {
      count *= 8
      elapsed = timeBatch(trial, sink, count)
    }
    math.max(elapsed.toDouble / count, 1e-3)
  }

  /** Time `count` invocations, returning elapsed nanos for all of them. */
  private def timeBatch(trial: Input.Staged, sink: Blackhole, count: Int): Long = {
    val batch = Input.stageBatch(trial, sink)
    try
      Input.runner(batch) match {
        case Input.Runner.Direct(state, body) =>
          val start = System.nanoTime()
          var i = 0
          while (i < count) {
            sink.consume(body(state))
            i += 1
          }
          System.nanoTime() - start

        case Input.Runner.Thunk(run) =>
          val start = System.nanoTime()
          var i = 0
          while (i < count) {
            sink.consume(run())
            i += 1
          }
          System.nanoTime() - start
      }
    finally Input.release(batch)
  }

  private def sized(targetNs: Double, perOpNs: Double): Int = clamp(targetNs / perOpNs)

  private def clamp(count: Double): Int =
    math.max(1.0, math.min(math.ceil(count), maxBatch.toDouble)).toInt

  /** A sample has to last this long to be worth extrapolating from, which is
    * several thousand times the clock's granularity.
    */
  private val calibrationNs: Long = 1000000L

  private val maxBatch: Int = 1 << 28

  /** A least squares fit of elapsed time against iteration count. */
  private final case class Fit(
      slope: Double,
      intercept: Double,
      slopeError: Double,
      rSquared: Double,
  )

  private object Fit {

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

  /** Two sided 95% critical values of Student's t, by degrees of freedom. */
  private val t95: Array[Double] = Array(12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 2.365, 2.306, 2.262, 2.228, 2.201,
    2.179, 2.160, 2.145, 2.131, 2.120, 2.110, 2.101, 2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 2.060, 2.056, 2.052,
    2.048, 2.045, 2.042)

  private def tValue(df: Int): Double =
    if (df <= 0) 0.0
    else if (df <= t95.length) t95(df - 1)
    else if (df <= 40) 2.021
    else if (df <= 60) 2.000
    else if (df <= 120) 1.980
    else 1.960

  object Defaults {

    /** Time spent warming up before any sample is kept, per platform. */
    val warmup: FiniteDuration = Platform.warmup

    /** Target duration of a single sample, when a batch is sized by time. */
    val sampleTime: FiniteDuration = 25.millis

    /** How many samples to keep. Enough that Student's t is not punishing. */
    val samples: Int = 30

    /** Let the harness size batches by default. */
    val batch: Batch = Batch.Timed(sampleTime)

    /** Report no throughput unless a bench declares its work. */
    val work: Work = Work.One

    /** One thread, so that a bench remains consistent across machines by
      * default.
      */
    val threads: Int = 1
  }
}
