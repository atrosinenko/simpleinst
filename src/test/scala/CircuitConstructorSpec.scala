package freechips.rocketchip.instrumenter

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.iotesters._
import chisel3.util.Cat
import freechips.rocketchip.instrumenter.BpfLoader.{BpfInsn, BpfProg}
import org.scalatest.{FlatSpec, Matchers}

object CircuitConstructorSpec {
  val MemLengthLg = 5
  val MemLength = 1 << MemLengthLg
  class DUT(program: BpfProg) extends Module {
    override val io = IO(new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())

      val arg1 = Input(UInt(64.W))
      val arg2 = Input(UInt(64.W))
      val arg3 = Input(UInt(64.W))

      val result = Output(UInt(64.W))

      val readAddr = Input(UInt(5.W))
      val readResult = Output(UInt(8.W))
    })

    val mem = SyncReadMem(MemLength, UInt(8.W))
    // Zero memory at the beginning of the simulation
    val memInitInd = RegInit(0.U((MemLengthLg + 1).W))
    memInitInd := Mux(memInitInd < MemLength.U, memInitInd + 1.U, memInitInd)
    when (memInitInd < MemLength.U) {
      mem.write(memInitInd, 0.U)
    }

    object Constructor extends BpfCircuitConstructor {
      override def doLoad(addr: UInt, lgsize: Int, valid: Bool): (UInt, Bool) = {
        val shiftedBytes = (0 until (1 << lgsize)).map { i =>
          mem.read(addr + i.U, valid).asUInt()
        }
        (Cat(shiftedBytes.reverse), RegNext(valid))
      }

      override def doStore(addr: UInt, lgsize: Int, data: UInt, valid: Bool): Bool = {
        (0 until (1 << lgsize)).foreach { i =>
          when (valid) {
            mem.write(addr + i.U, (data >> (i * 8))(7, 0))
          }
        }
        RegNext(valid)
      }

      override def resolveSymbol(sym: BpfLoader.Symbol): UInt = 12.U
    }

    val (result, resultValid) = Constructor.processProgram(
      io.valid,
      program.insns,
      io.arg1, io.arg2, io.arg3
    )

    io.readResult := mem.read(io.readAddr)
    io.result := result
    io.ready := resultValid

  }

  class CircuitTester(dut: DUT, result: BigInt, steps: Int, arg1: BigInt, arg2: BigInt, arg3: BigInt, memContents: Seq[Int]) extends PeekPokeTester(dut) {
    poke(dut.io.readAddr, 0)
    poke(dut.io.valid, false)
    poke(dut.io.arg1, arg1)
    poke(dut.io.arg2, arg2)
    poke(dut.io.arg3, arg3)

    step(100)

    expect(peek(dut.io.ready) == 0, "Circuit should not start on its own")

    poke(dut.io.valid, true)

    if (steps > 0) {
      step(steps - 1)
      expect(peek(dut.io.ready) == 0, s"Circuit should not finish its calculations before ${steps}th step")
    }

    step(1)

    expect(peek(dut.io.ready) == 1, s"Circuit should finish its calculations on ${steps}th step")

    val realResult = peek(dut.io.result)
    expect(realResult == result, s"Result should be $result, got $realResult")

    memContents.zipWithIndex.foreach {
      case (byte, ind) =>
        poke(dut.io.readAddr, ind)
        step(1)
        val realByte = peek(dut.io.readResult)
        expect((realByte & 0xFF) == (byte & 0xFF), s"Byte #$ind should be $byte, got $realByte")
    }
  }


}

class CircuitConstructorSpec extends FlatSpec with Matchers {
  import CircuitConstructorSpec._

  behavior of "CircuitConstructor"

  def testWith(insns: BpfInsn*)(args: (BigInt, BigInt, BigInt), result: BigInt, steps: Int, sparseMemContents: (Int, Int)*): Unit = {
    val mem = sparseMemContents.toMap
    val memContents = (0 until MemLength).map(mem.getOrElse(_, 0))
    chisel3.iotesters.Driver(() => new DUT(BpfProg("test", insns)), backendType="firrtl")(new CircuitTester(_, result, steps, args._1, args._2, args._3, memContents)) shouldBe true
  }

  def compileAndTest(text: String, args: (BigInt, BigInt), result: BigInt, steps: Int, sparseMemContents: (Int, Int)*): Unit = {
    val tempFile = Files.createTempFile("bpf", ".c")
    val fullText =
      s"""#include <stdint.h>
        |
        |int f(uint64_t x, uint64_t y) {
        |  $text
        |}
      """.stripMargin
    Files.write(tempFile, fullText.getBytes)
    val fname = tempFile.toAbsolutePath.toString
    Runtime.getRuntime
      .exec(Array("/bin/sh", "-c", s"clang -O3 -emit-llvm -c $fname -o - | llc -march=bpf -filetype=obj -o $fname.o"))
      .waitFor()
    val data = Files.readAllBytes(Paths.get(fname + ".o"))
    val insns = BpfLoader.fetchProgs(ByteBuffer.wrap(data)).head.insns
    testWith(insns: _*)((args._1, args._2, 0.bigint), result, steps, sparseMemContents: _*)
    Files.delete(tempFile)
    Files.delete(Paths.get(tempFile.toString + ".o"))
  }

  it should "return immediate values" in {
    testWith(
      BpfInsn(0x18, 0, 0, 0, Left(123))
    )((1, 2, 3), 123, 0)
  }

  it should "handle arithmetic on small numbers" in {
    testWith(
      BpfInsn(0x0f, 1, 2, 0, Left(0)), // r1 += r2
      BpfInsn(0x27, 1, 0, 0, Left(3)), // r1 *= 3
      BpfInsn(0x0f, 1, 3, 0, Left(0)), // r1 += r3
      BpfInsn(0x0f, 0, 1, 0, Left(0)), // r0 += r1
    )((1, 2, 3), 12, 0)
  }

  it should "handle 64-bit ALU" in {
    testWith(
      BpfInsn(0x0f, 1, 2, 0, Left(0)), // r1 += r2
      BpfInsn(0x27, 1, 0, 0, Left(3)), // r1 *= 3
      BpfInsn(0x2f, 1, 3, 0, Left(0)), // r1 *= r3
      BpfInsn(0x0f, 0, 1, 0, Left(0)), // r0 += r1
    )((1, 2, 0x80000000l), 0x480000000l, 0)
  }

  it should "handle 32-bit ALU" in {
    testWith(
      BpfInsn(0x0c, 1, 2, 0, Left(0)), // r1 += r2
      BpfInsn(0x24, 1, 0, 0, Left(3)), // r1 *= 3
      BpfInsn(0x2c, 1, 3, 0, Left(0)), // r1 *= r3
      BpfInsn(0x0c, 0, 1, 0, Left(0)), // r0 += r1
    )((1, 2, 0x80000000l), 0x80000000l, 0) // 72 == 0x48
  }

  it should "handle exit at the end" in {
    testWith(
      BpfInsn(0x0f, 1, 2, 0, Left(0)), // r1 += r2
      BpfInsn(0x0f, 0, 1, 0, Left(0)), // r0 += r1
      BpfInsn(0x95, 0, 0, 0, Left(0)), // exit
    )((1, 2, 3), 3, 0)
  }

  it should "write to memory" in {
    testWith(
      BpfInsn(0x72, 1, 0, 0, Left(0xabc)), // *(uint8_t *)(r1 + 0) = 0xbc
      BpfInsn(0xb4, 1, 0, 0, Left(10)),    // r1 = 10
      BpfInsn(0x73, 1, 3, 12, Left(0xabc)), // *(uint8_t *)(r1 + 12) = r3
    )((1, 2, 3), 0, 1,
      1 -> 0xbc,
      22 -> 3
    )
  }

  it should "load then store" in {
    testWith(
      BpfInsn(0x71, 1, 1, 1,  Left(0)),    // r1 = *(uint8_t *)(r1 + 1)
      BpfInsn(0x07, 1, 0, 0,  Left(0xab)), // r1 += 0xab
      BpfInsn(0x73, 2, 1, 10, Left(0)),    // *(uint8_t *)(r2 + 10) = r1
    )((1, 2, 3), 0, 2,
      12 -> 0xab
    )
  }

  it should "handle byteswap2" in {
    compileAndTest("return ((x & 0xFF) << 8) | ((x & 0xFF00) >> 8);",
      (0xabcdef, 0x00), 0xefcd, 0)
  }

  it should "handle bytemerge2" in {
    compileAndTest("return (x & 0xFF) | (y & 0xFF00);",
      (0xabcdef, 0x123456), 0x34ef, 0)
  }

  it should "handle popcount" in {
    compileAndTest("return __builtin_popcount(x);",
      (0x10000020AA000011l, 0), 6, 0)
    compileAndTest("return __builtin_popcountl(x);",
      (0x10000020AA000011l, 0), 8, 0)
  }
}
