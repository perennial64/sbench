package sbench

object Report {

  /** Whether to colorize, honouring the `NO_COLOR` convention. */
  val colorByDefault: Boolean = Platform.env("NO_COLOR").isEmpty

  /** One result, laid out as [[table]] lays it out. */
  def line(name: String, result: Bench.Result, color: Boolean = colorByDefault): String =
    render(entry(name, result, name.length), color)

  /** Every result, with names and timings aligned. */
  def table(results: Seq[(String, Bench.Result)], color: Boolean = colorByDefault): String =
    if (results.isEmpty) ""
    else {
      val width = results.map(_._1.length).max
      results.map { case (name, result) => render(entry(name, result, width), color) }.mkString("\n")
    }

  /** Print [[table]] to stdout. */
  def print(results: Seq[(String, Bench.Result)], color: Boolean = colorByDefault): Unit = {
    val text = table(results, color)
    if (text.nonEmpty) println(text)
  }

  private def render(str: fansi.Str, color: Boolean): String =
    if (color) str.render else str.plainText

  private def entry(name: String, result: Bench.Result, width: Int): fansi.Str = {
    val noisy = result.relStdDevPct >= 10.0 || result.outliers * 10 > result.samples
    val stats = fansi.Str(
      "median " + duration(result.medianNs) +
        "  sd " + duration(result.stdDevNs) + " (" + sig(result.relStdDevPct) + "%)" +
        "  " + count(result.samples) + " samples of " + count(result.batch) +
        (if (result.outliers > 0) "  " + count(result.outliers) + outlierWord(result.outliers)
         else "")
    )

    val rows = Seq.newBuilder[fansi.Str]

    rows += triple(
      "time:  ",
      duration(result.lowNs),
      duration(result.meanNs),
      duration(result.highNs),
    )

    for {
      low <- result.throughputAt(result.highNs)
      mean <- result.throughputAt(result.meanNs)
      high <- result.throughputAt(result.lowNs)
    } rows += triple("thrpt: ", rate(low, result.work), rate(mean, result.work), rate(high, result.work))

    rows +=
      (if (noisy) fansi.Color.Yellow(fansi.Str("stats:  ") ++ stats)
       else fansi.Color.DarkGray("stats:") ++ fansi.Str("  ") ++ stats)

    val lines = rows.result()
    val head = fansi.Bold.On(fansi.Str(pad(name, width))) ++ fansi.Str("  ") ++ lines.head
    lines.tail.foldLeft(head) { (acc, row) =>
      acc ++ fansi.Str("\n") ++ fansi.Str(pad("", width)) ++ fansi.Str("  ") ++ row
    }
  }

  private def triple(label: String, low: String, mean: String, high: String): fansi.Str =
    fansi.Str.join(
      Seq(
        fansi.Color.DarkGray(label.trim),
        fansi.Str(" " * (label.length - label.trim.length + 1)),
        fansi.Str("["),
        fansi.Str(low),
        fansi.Str(" "),
        fansi.Bold.On(fansi.Str(mean)),
        fansi.Str(" "),
        fansi.Str(high),
        fansi.Str("]"),
      )
    )

  private[sbench] def rate(unitsPerSecond: Double, work: Bench.Work): String = work match {
    case Bench.Work.Bytes(_) => binary(unitsPerSecond) + "/s"
    case _ => si(unitsPerSecond) + " ops/s"
  }

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

  private def outlierWord(outliers: Int): String =
    if (outliers == 1) " outlier" else " outliers"

  private def pad(text: String, width: Int): String =
    if (text.length >= width) text else text + " " * (width - text.length)

  private[sbench] def duration(ns: Double): String =
    if (ns < 1e3) sig(ns) + " ns"
    else if (ns < 1e6) sig(ns / 1e3) + " us"
    else if (ns < 1e9) sig(ns / 1e6) + " ms"
    else sig(ns / 1e9) + " s"

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
      val base = if (dot < 0) text + "." else text
      base + ("0" * (decimals - have))
    }
  }

  private[sbench] def count(value: Int): String = {
    val digits = value.toString
    val head = digits.length % 3
    val groups = (if (head == 0) Nil else List(digits.take(head))) ++
      digits.drop(head).grouped(3).toList
    groups.mkString(",")
  }
}
