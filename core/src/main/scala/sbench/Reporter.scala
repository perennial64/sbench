package sbench

import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Formatter of benchmark results.
  *
  * @param names
  *   Every name the suite may report
  * @param color
  *   Whether to colorize
  * @param detailed
  *   Whether to spell a result over several lines
  */
final class Reporter(
    val names: Seq[String],
    val color: Boolean = Reporter.colorByDefault,
    val detailed: Boolean = true,
) {

  private val width: Int = if (names.isEmpty) 0 else names.map(_.length).max

  /** One measured benchmark, or the failure in its place. */
  def line(measured: (String, Try[Bench.Result])): String = {
    val (name, result) = measured
    val rendered = result match {
      case Success(value) => if (detailed) detail(name, value) else compact(name, value)
      case Failure(error) =>
        Reporter.join(
          Seq(
            fansi.Bold.On(fansi.Str(Reporter.pad(name, width))),
            fansi.Str("  "),
            fansi.Color.Red("failed: " + Option(error.getMessage).getOrElse(error.toString)),
          )
        )
    }
    if (color) rendered.render else rendered.plainText
  }

  /** What a completed run amounted to. */
  def summary(measured: Seq[(String, Try[Bench.Result])]): String = {
    val failed = measured.count(_._2.isFailure)
    val text =
      Reporter.count(measured.length) + (if (measured.length == 1) " benchmark" else " benchmarks") +
        (if (failed > 0) ", " + Reporter.count(failed) + " failed" else "")
    val rendered = if (failed > 0) fansi.Color.Red(text) else fansi.Str(text)
    if (color) rendered.render else rendered.plainText
  }

  /** Several lines: the estimate with its interval, then what it rests on. */
  private def detail(name: String, result: Bench.Result): fansi.Str = {
    val rows = Seq.newBuilder[fansi.Str]

    rows += triple(
      "time:  ",
      Reporter.duration(result.lowNs),
      Reporter.duration(result.estimateNs),
      Reporter.duration(result.highNs),
    )

    // The slowest time is the lowest throughput, so the bounds swap over.
    for {
      low <- result.throughputAt(result.highNs)
      mid <- result.throughputAt(result.estimateNs)
      high <- result.throughputAt(result.lowNs)
    } rows += triple(
      "thrpt: ",
      Reporter.rate(low, result.work),
      Reporter.rate(mid, result.work),
      Reporter.rate(high, result.work),
    )

    val stats = fansi.Str(
      "median " + Reporter.duration(result.medianNs) +
        "  sd " + Reporter.duration(result.stdDevNs) +
        " (" + Reporter.sig(result.relStdDevPct) + "%)" + outlierText(result.outliers)
    )
    rows +=
      (if (unsettled(result)) fansi.Color.Yellow(fansi.Str("stats:  ") ++ stats)
       else fansi.Color.DarkGray("stats:") ++ fansi.Str("  ") ++ stats)

    rows += fansi.Color.DarkGray("plan:") ++ fansi.Str("   ") ++ fansi.Str(plan(result))

    val lines = rows.result()
    val head = fansi.Bold.On(fansi.Str(Reporter.pad(name, width))) ++ fansi.Str("  ") ++ lines.head
    lines.tail.foldLeft(head)((acc, row) =>
      acc ++ fansi.Str("\n") ++ fansi.Str(Reporter.pad("", width)) ++ fansi.Str("  ") ++ row
    )
  }

  /** One line: the estimate, how far it is to be trusted, and what it took. */
  private def compact(name: String, result: Bench.Result): fansi.Str = {
    val error =
      if (result.estimateNs == 0.0) ""
      else " +-" + Reporter.sig(result.errorNs / result.estimateNs * 100.0) + "%"
    val trailer =
      result.throughput.map(rate => "  " + Reporter.rate(rate, result.work)).getOrElse("") +
        outlierText(result.outliers)

    Reporter.join(
      Seq(
        fansi.Bold.On(fansi.Str(Reporter.pad(name, width))),
        fansi.Str("  "),
        fansi.Bold.On(fansi.Str(Reporter.duration(result.estimateNs))),
        if (unsettled(result)) fansi.Color.Yellow(fansi.Str(error)) else fansi.Str(error),
        fansi.Color.DarkGray("  " + plan(result)),
        if (unsettled(result)) fansi.Color.Yellow(fansi.Str(trailer)) else fansi.Str(trailer),
      )
    )
  }

  private def triple(label: String, low: String, mid: String, high: String): fansi.Str =
    Reporter.join(
      Seq(
        fansi.Color.DarkGray(label.trim),
        fansi.Str(" " * (label.length - label.trim.length + 1)),
        fansi.Str("["),
        fansi.Str(low),
        fansi.Str(" "),
        fansi.Bold.On(fansi.Str(mid)),
        fansi.Str(" "),
        fansi.Str(high),
        fansi.Str("]"),
      )
    )

  private def plan(result: Bench.Result): String = {
    val iterations = result.samples.map(_.iterations)
    val counts =
      if (iterations.min == iterations.max) Reporter.count(iterations.min)
      else Reporter.count(iterations.min) + ".." + Reporter.count(iterations.max)
    Reporter.count(result.sampleCount) + "x" + counts +
      (if (result.threads > 1) " on " + Reporter.count(result.threads) + " threads" else "") +
      result.rSquared.fold("")(r => "  R2 " + Reporter.sig(r))
  }

  private def outlierText(outliers: Bench.Outliers): String =
    if (outliers.total == 0) ""
    else {
      val parts = Seq(
        if (outliers.mild > 0) Some(Reporter.count(outliers.mild) + " mild") else None,
        if (outliers.severe > 0) Some(Reporter.count(outliers.severe) + " severe") else None,
      ).flatten
      "  " + parts.mkString(", ") + " outlier" + (if (outliers.total == 1) "" else "s")
    }

  /** Whether the numbers never settled, which is the one judgement a report can
    * make without a baseline to compare against.
    */
  private def unsettled(result: Bench.Result): Boolean =
    result.relStdDevPct >= 10.0 ||
      result.outliers.total * 10 > result.sampleCount ||
      result.rSquared.exists(_ < 0.99)
}

object Reporter {

  /** Beyond this many benchmarks a reporter defaults to one line each. */
  val detailThreshold: Int = 20

  /** Whether to colorize, honouring the `NO_COLOR` convention. */
  val colorByDefault: Boolean = Platform.env("NO_COLOR").isEmpty

  /** A reporter for a suite, spelling results out only for a short one. */
  def of(suite: Suite, color: Boolean = colorByDefault): Reporter =
    new Reporter(suite.names, color, suite.names.length <= detailThreshold)

  private def join(parts: Seq[fansi.Str]): fansi.Str = fansi.Str.join(parts)

  private def pad(text: String, width: Int): String =
    if (text.length >= width) text else text + " " * (width - text.length)

  /** Nanoseconds in the largest unit that keeps the value above one. */
  private[sbench] def duration(ns: Double): String =
    if (ns < 1e3) sig(ns) + " ns"
    else if (ns < 1e6) sig(ns / 1e3) + " us"
    else if (ns < 1e9) sig(ns / 1e6) + " ms"
    else sig(ns / 1e9) + " s"

  /** Work units per second, scaled by the kind of unit. */
  private[sbench] def rate(unitsPerSecond: Double, work: Bench.Work): String = work match {
    case Bench.Work.Bytes(_) => binary(unitsPerSecond) + "/s"
    case _ => si(unitsPerSecond) + " ops/s"
  }

  /** Bytes grow in powers of two, as criterion reports them. */
  private def binary(value: Double): String =
    if (value < 1024.0) sig(value) + " B"
    else if (value < 1024.0 * 1024) sig(value / 1024.0) + " KiB"
    else if (value < 1024.0 * 1024 * 1024) sig(value / (1024.0 * 1024)) + " MiB"
    else sig(value / (1024.0 * 1024 * 1024)) + " GiB"

  private def si(value: Double): String =
    if (value < 1e3) sig(value)
    else if (value < 1e6) sig(value / 1e3) + "K"
    else if (value < 1e9) sig(value / 1e6) + "M"
    else sig(value / 1e9) + "G"

  /** Four significant digits, without using a JVM-specific
    * `java.util.Formatter`.
    */
  private[sbench] def sig(value: Double): String = {
    val decimals =
      if (value >= 1e3) 0
      else if (value >= 1e2) 1
      else if (value >= 1e1) 2
      else 3
    val factor = math.pow(10, decimals.toDouble)
    val rounded = math.round(value * factor) / factor
    if (decimals == 0) math.round(rounded).toString
    else {
      val text = rounded.toString
      val dot = text.indexOf('.')
      val have = if (dot < 0) 0 else text.length - dot - 1
      (if (dot < 0) text + "." else text) + ("0" * (decimals - have))
    }
  }

  /** Thousands separated, so counts stay readable. */
  private[sbench] def count(value: Long): String = {
    val digits = value.toString
    val head = digits.length % 3
    val groups =
      (if (head == 0) Nil else List(digits.take(head))) ++ digits.drop(head).grouped(3).toList
    groups.mkString(",")
  }
}
