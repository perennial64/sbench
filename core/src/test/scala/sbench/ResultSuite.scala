package sbench

class ResultSuite extends munit.FunSuite {

  private def flat(perOp: Double*): Bench.Result =
    Bench.Result(perOp.toVector.map(ns => Bench.Sample(1L, ns.toLong)))

  test("a ramp separates the per-invocation cost from the cost of a sample") {
    // 100ns an invocation, plus 500ns of fixed cost per sample
    val ramped = Bench.Result(
      Vector(10L, 20L, 30L, 40L).map(n => Bench.Sample(n, n * 100 + 500))
    )
    assert(ramped.ramped)
    assertEqualsDouble(ramped.estimateNs, 100.0, 1e-9)
    assertEquals(ramped.overheadNs.map(_.round), Some(500L))
    assertEqualsDouble(ramped.rSquared.get, 1.0, 1e-9)
    // the plain mean still carries that fixed cost, which is what the fit removes
    assert(ramped.meanNs > 110.0)
  }

  test("samples of one size are summarised, not fitted") {
    val result = flat(10, 12, 14)
    assert(!result.ramped)
    assertEquals(result.overheadNs, None)
    assertEquals(result.rSquared, None)
    assertEqualsDouble(result.estimateNs, result.meanNs, 1e-9)
  }

  test("the interval widens as samples get scarcer") {
    val few = flat(10, 12, 14)
    val many = flat(List.fill(10)(Seq(10.0, 12.0, 14.0)).flatten: _*)
    // same spread, more samples: both the t value and the root of n shrink it
    assert(clue(few.errorNs) > clue(many.errorNs) * 2)
    assert(few.lowNs < few.estimateNs && few.estimateNs < few.highNs)
  }

  test("outliers are classified by distance, not just counted") {
    val result = flat(10, 11, 12, 13, 14, 15, 16, 17, 25, 40)
    val outliers = result.outliers
    assertEquals(outliers, Bench.Outliers(lowSevere = 0, lowMild = 0, highMild = 1, highSevere = 1))
    assertEquals(outliers.total, 2)
  }

  test("percentiles pick samples by rank, including the ends") {
    val result = flat(10, 20, 30, 40)
    assertEquals(result.percentileNs(0), result.minNs)
    assertEquals(result.percentileNs(100), result.maxNs)
    assertEquals(result.medianNs, 20.0)
  }

  test("throughput follows the declared work, and scales with threads") {
    val samples = Vector(Bench.Sample(1L, 1000L)) // 1us an invocation
    assertEquals(Bench.Result(samples).throughput, None)
    assertEquals(Bench.Result(samples, work = Bench.Work.Ops(5)).throughput, Some(5e6))
    assertEquals(Bench.Result(samples, work = Bench.Work.Bytes(1000)).throughput, Some(1e9))
    // latency is per thread, so the rate multiplies where the time does not
    val threaded = Bench.Result(samples, threads = 4)
    assertEquals(threaded.invocationsPerSecond, 4e6)
    assertEquals(Bench.Result(samples).invocationsPerSecond, 1e6)
  }

  test("a result refuses to describe nothing") {
    intercept[IllegalArgumentException](Bench.Result(Vector.empty))
    intercept[IllegalArgumentException](Bench.Sample(0L, 10L))
  }
}
