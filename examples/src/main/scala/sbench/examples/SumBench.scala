package sbench.examples

import sbench.Bench
import sbench.Input
import sbench.Suite

/** Benches summing an array of numbers. */
object SumBench extends Suite {

  bench(
    "sum of 1M ints",
    Input.perTrial(Array.range(0, 1_000_000)),
    Bench.Options(runs = 10, warmup = 3),
  ) { numbers =>
    numbers.sum
  }
}

@main def runSumBench(): Unit = SumBench.run()
