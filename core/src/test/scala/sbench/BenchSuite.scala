package sbench

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.DurationInt

class BenchSuite extends munit.FunSuite {

  private val quick = Bench.Options(warmup = 0.millis, samples = 4, batch = Bench.Batch.Fixed(8))

  private def counted: (AtomicInteger, Input[Input.Trial, Int]) = {
    val calls = new AtomicInteger(0)
    (calls, for { n <- Input.perTrial(7) } yield { calls.incrementAndGet(); n * 2 })
  }

  test("a fixed batch is taken literally, a timed one is sized but uniform") {
    val fixed = Bench.bench(counted._2, quick)
    assertEquals(fixed.samples.map(_.iterations).distinct, Vector(8L))

    val timed = Bench.bench(counted._2, quick.copy(batch = Bench.Batch.Timed(5.millis)))
    assertEquals(timed.samples.map(_.iterations).distinct.size, 1)
    assert(clue(timed.samples.head.iterations) > 8L)
  }

  test("a ramped batch varies the count and does not simply ascend") {
    val result =
      Bench.bench(counted._2, quick.copy(samples = 12, batch = Bench.Batch.Ramped(5.millis)))
    val counts = result.samples.map(_.iterations)
    assert(result.ramped)
    assert(clue(counts.distinct.size) > 1)
    assertNotEquals(counts, counts.sorted, "a ramp tied to sample order biases the fit")
  }

  test("every thread runs the whole plan, and says so") {
    assume(Platform.supportsThreads, "this platform runs a single thread")
    val threads = 3
    val states = new AtomicInteger(0)
    val result = Bench.bench(
      for {
        shared <- Input.perTrial(new AtomicInteger(0))
        _ <- Input.perThread(states.incrementAndGet())
      } yield shared.incrementAndGet(),
      quick.copy(threads = threads),
    )
    assertEquals(result.threads, threads)
    assertEquals(result.sampleCount, quick.samples * threads)
    // one per worker, plus the one the preparing thread builds to verify with
    assertEquals(states.get, threads + 1)
  }

  test("a failing check stops a bench before it measures anything") {
    val (calls, chain) = counted
    intercept[Input.VerificationFailed] {
      Bench.bench(chain.verify("never")(_ => false), quick)
    }
    assertEquals(calls.get, 1)
  }

  test("nonsense configuration is refused where it is written") {
    intercept[IllegalArgumentException](Bench.Options(samples = 0))
    intercept[IllegalArgumentException](Bench.Options(warmup = (-1).second))
    intercept[IllegalArgumentException](Bench.Options(threads = 0))
    intercept[IllegalArgumentException](Bench.Batch.Fixed(0))
    intercept[IllegalArgumentException](Bench.Batch.Timed(0.millis))
    intercept[IllegalArgumentException](Bench.Batch.Ramped((-1).millis))
    intercept[IllegalArgumentException](Bench.Work.Bytes(0))
  }

  test("threads beyond the core count are allowed, unsupported ones are not") {
    if (Platform.supportsThreads) {
      val many = Bench.Options(threads = Platform.cores * 2)
      assertEquals(many.threads, Platform.cores * 2)
    } else
      intercept[IllegalArgumentException](Bench.Options(threads = 2))
  }
}
