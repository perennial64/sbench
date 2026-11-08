package sbench

object Report {

  /** Whether to colorize, honouring the `NO_COLOR` convention. */
  val colorByDefault: Boolean =
    try !sys.env.contains("NO_COLOR")
    catch { case _: Throwable => false }

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
    val spread = result.maxNs / result.minNs
    val detail =
      s"${count(result.batch)} per sample, ${count(result.runs)} samples, max ${sig(spread)}x min"

    fansi.Str.join(
      Seq(
        fansi.Bold.On(fansi.Str(pad(name, width))),
        fansi.Str("  "),
        fansi.Color.DarkGray("time:   ["),
        fansi.Color.DarkGray(duration(result.minNs)),
        fansi.Str(" "),
        fansi.Bold.On(fansi.Str(duration(result.meanNs))),
        fansi.Str(" "),
        fansi.Color.DarkGray(duration(result.maxNs)),
        fansi.Color.DarkGray("]"),
        fansi.Str("\n"),
        fansi.Str(pad("", width)),
        fansi.Str("  "),
        // A wide spread means the benchmark never settled
        if (spread >= 2.0) fansi.Color.Yellow("spread: " + detail)
        else fansi.Color.DarkGray("spread: " + detail),
      )
    )
  }

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
