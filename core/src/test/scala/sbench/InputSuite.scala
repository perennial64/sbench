package sbench

import scala.collection.mutable.ListBuffer

class InputSuite extends munit.FunSuite {

  /** Drives the staging facade the way Bench does. */
  private def drive[L <: Input.Lvl, A](
      chain: Input[L, A],
      threads: Int = 1,
      batches: Int = 1,
      perBatch: Int = 1,
      verify: Boolean = true,
  ): Unit = {
    val trial = Input.stageTrial(chain, new Blackhole())
    try {
      if (verify) {
        val sink = new Blackhole()
        val staged = Input.stageThread(trial, sink)
        try Input.verifyOnce(staged, sink)
        finally Input.release(staged)
      }
      for (_ <- 0 until threads) {
        val sink = new Blackhole()
        val thread = Input.stageThread(trial, sink)
        try
          for (_ <- 0 until batches) {
            val batch = Input.stageBatch(thread, sink)
            try
              Input.runner(batch) match {
                case Input.Runner.Direct(state, body) =>
                  for (_ <- 0 until perBatch) sink.consume(body(state))
                case Input.Runner.Thunk(run) =>
                  for (_ <- 0 until perBatch) sink.consume(run())
              }
            finally Input.release(batch)
          }
        finally Input.release(thread)
      }
    } finally Input.release(trial)
  }

  test("each level is rebuilt at its own cadence") {
    val built = ListBuffer.empty[String]
    drive(
      for {
        _ <- Input.perTrial(built += "trial")
        _ <- Input.perThread(built += "thread")
        _ <- Input.perBatch(built += "batch")
        _ <- Input.perInvocation(built += "invocation")
      } yield (),
      threads = 2,
      batches = 3,
      perBatch = 4,
      verify = false,
    )
    assertEquals(built.count(_ == "trial"), 1)
    assertEquals(built.count(_ == "thread"), 2)
    assertEquals(built.count(_ == "batch"), 2 * 3)
    assertEquals(built.count(_ == "invocation"), 2 * 3 * 4)
  }

  test("trial state is shared across threads, finer state is not") {
    val shared = new ListBuffer[Int]
    val perThreadSizes = ListBuffer.empty[Int]
    drive(
      for {
        trial <- Input.perTrial(shared)
        own <- Input.perThread(ListBuffer.empty[Int])
      } yield {
        trial += 1
        own += 1
        perThreadSizes += own.size
      },
      threads = 3,
      perBatch = 2,
      verify = false,
    )
    assertEquals(shared.size, 6)
    assertEquals(perThreadSizes.toList, List(1, 2, 1, 2, 1, 2))
  }

  test("state is released once per build, finest first") {
    val log = ListBuffer.empty[String]
    drive(
      for {
        _ <- Input.perTrial("a", (_: String) => log += "a")
        _ <- Input.perBatch("b", (_: String) => log += "b")
        _ <- Input.perInvocation("c", (_: String) => log += "c")
      } yield (),
      batches = 2,
      perBatch = 2,
      verify = false,
    )
    assertEquals(log.toList, List("c", "c", "b", "c", "c", "b", "a"))
  }

  /** Declared as Int so that a chain ending in it still infers on Scala 2. */
  private def boom(): Int = throw new RuntimeException("boom")

  test("a throwing body still releases what the coarser stages built") {
    val log = ListBuffer.empty[String]
    intercept[RuntimeException] {
      drive(
        for {
          _ <- Input.perTrial("a", (_: String) => log += "a")
          _ <- Input.perBatch("b", (_: String) => log += "b")
        } yield boom(),
        verify = false,
      )
    }
    assertEquals(log.toList, List("b", "a"))
  }

  test("each thread gets its own blackhole, the same one throughout") {
    val seen = ListBuffer.empty[Int]
    drive(
      for {
        sink <- Input.blackhole
      } yield seen += System.identityHashCode(sink),
      threads = 2,
      perBatch = 3,
      verify = false,
    )
    assertEquals(seen.distinct.size, 2)
    assertEquals(seen.size, 6)
  }

  test("checks run once, outside the loop, and the body still runs per invocation") {
    var checks = 0
    var calls = 0
    drive(
      (for { n <- Input.perTrial(7) } yield { calls += 1; n * 2 })
        .verify("doubled") { v => checks += 1; v == 14 },
      batches = 2,
      perBatch = 5,
    )
    assertEquals(checks, 1)
    assertEquals(calls, 1 + 2 * 5)
  }

  test("a failing check names itself and aborts") {
    val failure = intercept[Input.VerificationFailed] {
      drive((for { n <- Input.perTrial(1) } yield n).verify("is 99")(_ == 99))
    }
    assertEquals(failure.label, "is 99")
  }

  test("checks leave the timed path alone") {
    def runnerOf[L <: Input.Lvl, A](chain: Input[L, A]): Input.Runner = {
      val sink = new Blackhole()
      Input.runner(Input.stageBatch(Input.stageThread(Input.stageTrial(chain, sink), sink), sink))
    }
    val plain = for { n <- Input.perTrial(7) } yield n * 2
    val checked = plain.verify("positive")(_ > 0)
    assert(runnerOf(plain).isInstanceOf[Input.Runner.Direct])
    assert(runnerOf(checked).isInstanceOf[Input.Runner.Direct])
    // a per-invocation input has to be built inside the loop instead
    val fresh = for { xs <- Input.perInvocation(new Array[Int](1)) } yield xs.length
    assert(runnerOf(fresh).isInstanceOf[Input.Runner.Thunk])
  }
}
