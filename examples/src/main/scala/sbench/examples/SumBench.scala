package sbench.examples

import sbench.Bench
import sbench.Input
import sbench.Suite

object SumBench extends Suite {

  bench(
    "sum of 1M ints",
    Bench.Options(runs = 10, warmup = 3),
    Input.perTrial(Array.range(0, 1_000_000)),
  ) { numbers =>
    numbers.sum
  }
}
