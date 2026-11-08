package sbench.examples

import sbench.Report

/** Entry point for every example suite. */
@main def runExamples(): Unit =
  Report.print(SumBench.run() ++ BatchBench.run() ++ LookupBench.run())
