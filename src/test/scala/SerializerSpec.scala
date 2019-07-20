package freechips.rocketchip.instrumenter

import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}

object SerializerSpec {
  class DUT extends Module {
    override val io = IO(new Bundle {
      val isComputing = Input(Bool())
      val next = Input(Bool())

      val req1 = Input(Bool())
      val req2 = Input(Bool())
      val req3 = Input(Bool())
      val req4 = Input(Bool())

      val resp1 = Output(Bool())
      val resp2 = Output(Bool())
      val resp3 = Output(Bool())
      val resp4 = Output(Bool())
    })
    val serializer = new Serializer(io.isComputing, io.next)
    io.resp1 := serializer.nextReq(io.req1)._1
    io.resp2 := serializer.nextReq(io.req2)._1
    io.resp3 := serializer.nextReq(io.req3)._1
    io.resp4 := serializer.nextReq(io.req4)._1
  }

  final case class Session(start: Int, reqs: T, resps: T, nexts: T, stop: Int)
  final case class T(a: Int, b: Int, c: Int, d: Int) {
    def contains(t: Int): Boolean = Seq(a, b, c, d).contains(t)
  }

  class Tester(dut: DUT, sessions: Seq[Session]) extends PeekPokeTester(dut) {
    var sessionInd = 0
    def curSession = sessions(sessionInd)
    for (t <- 0 until sessions.last.stop) {
      if (t >= curSession.stop) {
        sessionInd += 1
      }
      poke(dut.io.isComputing, t >= curSession.start)

      poke(dut.io.req1, t >= curSession.reqs.a)
      poke(dut.io.req2, t >= curSession.reqs.b)
      poke(dut.io.req3, t >= curSession.reqs.c)
      poke(dut.io.req4, t >= curSession.reqs.d)

      poke(dut.io.next, curSession.nexts.contains(t))

      step(1)

      def test(x: Bool, t1: Int, t2: Int, label: String): Unit = {
        val expected = (t + 1) >= t1 && (t + 1) <= t2
        expect((peek(x) != 0) == expected, s"$label should be $expected")
      }
      test(dut.io.resp1, curSession.resps.a, curSession.nexts.a, "Response 1")
      test(dut.io.resp2, curSession.resps.b, curSession.nexts.b, "Response 2")
      test(dut.io.resp3, curSession.resps.c, curSession.nexts.c, "Response 3")
      test(dut.io.resp4, curSession.resps.d, curSession.nexts.d, "Response 4")

    }
  }
}

class SerializerSpec extends FlatSpec with Matchers {
  import SerializerSpec._

  behavior of "Serializer"

  def testWith(sessions: Session*): Unit = {
    chisel3.iotesters.Driver(() => new DUT, backendType="firrtl")(new Tester(_, sessions)) shouldBe true
  }

  it should "pass through sequential requests" in {
    testWith(Session(0, T(1, 2, 3, 4), T(2, 3, 4, 5), T(2, 3, 4, 5), 10))
  }
  it should "handle all-simultaneous request" in {
    testWith(Session(0, T(1, 1, 1, 1), T(2, 3, 4, 5), T(2, 3, 4, 5), 10))
  }
  it should "handle simultaneous request" in {
    testWith(Session(0, T(1, 1, 7, 7), T(2, 3, 8, 9), T(2, 3, 8, 9), 10))
  }
  it should "handle queued request" in {
    testWith(Session(0, T(1, 1, 2, 3), T(2, 3, 4, 5), T(2, 3, 4, 5), 10))
  }
  it should "handle not-immediate processing" in {
    testWith(Session(0, T(1, 1, 1, 1), T(2, 4, 5, 8), T(3, 4, 7, 9), 10))
  }
  it should "handle multiple sessions" in {
    testWith(
      Session(0, T(1, 1, 1, 1), T(2, 4, 5, 118), T(3, 4, 117, 119), 5), // the last resp is not asserted this session
      Session(6, T(7, 7, 15, 15), T(8, 9, 16, 17), T(8, 9, 16, 17), 20)
    )
  }
}
