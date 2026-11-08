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

    /** Size each sample to take about `sampleTime`, measured after warmup. */
    final case class Timed(sampleTime: FiniteDuration = Defaults.sampleTime) extends Batch {
      require(sampleTime.toNanos > 0, "sampleTime must be positive")
    }

    /** Time exactly `size` invocations per sample. */
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

  /** Result of a bench: one per-invocation timing for each sample taken. */
  final case class Result(batch: Int, samplesNs: Vector[Double], work: Work = Work.One) {
    require(samplesNs.nonEmpty, "a result needs at least one sample")

    private lazy val sorted: Vector[Double] = samplesNs.sorted

    def samples: Int = samplesNs.length
    def meanNs: Double = samplesNs.sum / samples
    def minNs: Double = sorted.head
    def maxNs: Double = sorted.last
    def medianNs: Double = percentileNs(50)

    /** The `p`th percentile by nearest rank. */
    def percentileNs(p: Double): Double = {
      val rank = math.ceil(p / 100.0 * samples).toInt - 1
      sorted(math.min(samples - 1, math.max(0, rank)))
    }

    /** Sample standard deviation, zero when there is nothing to vary. */
    def stdDevNs: Double =
      if (samples < 2) 0.0
      else {
        val mean = meanNs
        math.sqrt(samplesNs.map(s => (s - mean) * (s - mean)).sum / (samples - 1))
      }

    /** Standard deviation as a share of the mean, the handiest noise measure.
      */
    def relStdDevPct: Double = if (meanNs == 0.0) 0.0 else stdDevNs / meanNs * 100.0

    /** Half width of a 95% confidence interval of the mean, by normal
      * approximation.
      */
    def errorNs: Double = 1.96 * stdDevNs / math.sqrt(samples.toDouble)

    def lowNs: Double = meanNs - errorNs
    def highNs: Double = meanNs + errorNs

    /** Samples beyond Tukey's fences. */
    def outliers: Int = {
      val q1 = percentileNs(25)
      val q3 = percentileNs(75)
      val fence = 1.5 * (q3 - q1)
      samplesNs.count(s => s < q1 - fence || s > q3 + fence)
    }

    /** Invocations per second, whatever work each one does. */
    def invocationsPerSecond: Double = if (meanNs == 0.0) 0.0 else 1e9 / meanNs

    /** Work units per second at a given per-invocation time, or `None` when no
      * work was declared.
      */
    def throughputAt(ns: Double): Option[Double] = work match {
      case Work.One => None
      case _ if ns <= 0.0 => None
      case Work.Ops(count) => Some(count * 1e9 / ns)
      case Work.Bytes(count) => Some(count * 1e9 / ns)
    }

    /** Work units per second at the mean. */
    def throughput: Option[Double] = throughputAt(meanNs)
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
      val cold = sizeBatch(trial.node, opts.batch)
      warmUp(trial.node, cold, opts.warmup.toNanos)
      val batch = sizeBatch(trial.node, opts.batch)

      val samples = Vector.newBuilder[Double]
      var s = 0
      while (s < opts.samples) {
        samples += timeBatch(trial.node, batch).toDouble / batch
        s += 1
      }

      Result(batch, samples.result(), opts.work)
    } finally Input.releaseAll(trial.releases)
  }

  /** Run batches until `warmupNs` has elapsed. */
  private def warmUp(trial: Input.Node, batch: Int, warmupNs: Long): Unit = {
    val deadline = System.nanoTime() + warmupNs
    while (System.nanoTime() < deadline) {
      timeBatch(trial, batch)
      ()
    }
  }

  /** Pick a batch size whose sample lasts about [[Batch.Timed.sampleTime]]. */
  private def sizeBatch(trial: Input.Node, batching: Batch): Int = batching match {
    case Batch.Fixed(size) => size

    case Batch.Timed(sampleTime) =>
      var batch = 1
      var elapsed = timeBatch(trial, batch)
      while (elapsed < Defaults.calibrationNs && batch <= Defaults.maxBatch / 8) {
        batch *= 8
        elapsed = timeBatch(trial, batch)
      }
      val perOpNs = math.max(elapsed.toDouble / batch, 1e-3)
      val scaled = math.ceil(sampleTime.toNanos / perOpNs)
      math.max(1.0, math.min(scaled, Defaults.maxBatch.toDouble)).toInt
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

    /** Target duration of a single sample, when [[Batch.Timed]] sizes it. */
    val sampleTime: FiniteDuration = 100.millis

    /** How many samples to keep. */
    val samples: Int = 10

    /** Report no throughput unless a bench declares its work. */
    val work: Work = Work.One

    /** Let the harness size batches by default. */
    val batch: Batch = Batch.Timed()

    /** A calibration sample has to last this long to be worth extrapolating
      * from.
      */
    private[sbench] val calibrationNs: Long = 1000000L

    private[sbench] val maxBatch: Int = 1 << 28
  }
}
