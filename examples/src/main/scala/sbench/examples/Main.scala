package sbench.examples

import scala.util.Try

import sbench.Bench
import sbench.Reporter
import sbench.Suite

@main def runExamples(): Unit = {
  val suites: Vector[(String, Suite)] = Vector(
    "Basics" -> Basics,
    "Batching" -> Batching,
    "State" -> State,
    "Blackholes" -> Blackholes,
    "Throughput" -> Throughput,
    "Threads" -> Threads,
    "Checks" -> Checks,
  )

  val reporter = new Reporter(suites.flatMap(_._2.names))
  val measured = Vector.newBuilder[(String, Try[Bench.Result])]

  for ((label, suite) <- suites) {
    println(s"-- $label --")
    for (result <- suite.collect()) {
      println(reporter.line(result))
      measured += result
    }
  }

  println(reporter.summary(measured.result()))
}
