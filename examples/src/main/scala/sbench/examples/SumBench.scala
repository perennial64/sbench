package sbench.examples

import sbench.Bench
import sbench.Suite

/** Benches summing an array of numbers. */
object SumBench extends Suite {

  private val numbers: Array[Int] = Array.range(0, 1_000_000)

  bench("sum of 1M ints", Bench.Options(runs = 10, warmup = 3)) {
    numbers.sum
  }
}

@main def runSumBench(): Unit = SumBench.run()
