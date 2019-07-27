package freechips.rocketchip.instrumenter

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.iotesters._
import chisel3.util.Cat
import freechips.rocketchip.instrumenter.BpfLoader.{BpfInsn, BpfProg}
import freechips.rocketchip.util._
import org.scalatest.{FlatSpec, Matchers}

object CircuitConstructorSpec {
  val MemLengthLg = 5
  val MemLength = 1 << MemLengthLg
  val MemInit = 0xaa
  class DUT(programs: Seq[Seq[BpfInsn]]) extends Module {
    override val io = IO(new Bundle {

      val arg1 = Input(UInt(64.W))
      val arg2 = Input(UInt(64.W))
      val arg3 = Input(UInt(64.W))

      val valid = Input(Vec(programs.length, Bool()))
      val ready = Output(Vec(programs.length, Bool()))
      val result = Output(Vec(programs.length, UInt(64.W)))

      val readAddr = Input(UInt(5.W))
      val readResult = Output(UInt(8.W))
    })

    val mem = SyncReadMem(MemLength, UInt(8.W))
    // Zero memory at the beginning of the simulation
    val memInitInd = RegInit(0.U((MemLengthLg + 1).W))
    memInitInd := Mux(memInitInd < MemLength.U, memInitInd + 1.U, memInitInd)
    when (memInitInd < MemLength.U) {
      mem.write(memInitInd, MemInit.U)
    }

    object Constructor extends BpfCircuitConstructor {
      override def doMemLoad(addr: UInt, tpe: LdStType, valid: Bool): (UInt, Bool) = {
        val shiftedBytes = (0 until (1 << tpe.lgsize)).map { i =>
          mem.read(addr + i.U, valid).asUInt()
        }
        when (RegNext(posedge(valid))) {
          printf("load %d: %x\n", addr, Cat(shiftedBytes.reverse))
        }
        (Cat(shiftedBytes.reverse) holdUnless RegNext(posedge(valid)), RegNext(RegNext(RegNext(valid))))
      }

      override def doMemStore(addr: UInt, tpe: LdStType, data: UInt, valid: Bool): Bool = {
        (0 until (1 << tpe.lgsize)).foreach { i =>
          when (posedge(valid)) {
            mem.write(addr + i.U, (data >> (i * 8))(7, 0))
          }
        }
        when (posedge(valid)) {
          printf("store %d <- %x\n", addr, data)
        }
        RegNext(RegNext(RegNext(valid)))
      }

      val regA = RegInit(0.U(64.W))
      val regB = RegInit(0.U(64.W))
      val regC = RegInit(0.U(64.W))
      val regD = RegInit(0.U(64.W))
      val regAddr = RegInit(0.U(64.W))
      override def resolveSymbol(sym: BpfLoader.Symbol): Resolved = sym match {
        case BpfLoader.Symbol("a", _, _, _, _) =>
          RegisterReference(regA)
        case BpfLoader.Symbol("b", _, _, _, _) =>
          RegisterReference(regB)
        case BpfLoader.Symbol("c", _, _, _, _) =>
          RegisterReference(regC)
        case BpfLoader.Symbol("d", _, _, _, _) =>
          RegisterReference(regC)
        case BpfLoader.Symbol("addr", _, _, _, _) =>
          RegisterReference(regAddr)
        case BpfLoader.Symbol(_, value, _, shndx, _) if shndx != ElfConstants.Elf64_Shdr.SHN_COMMON =>
          Value(value.U)
      }
    }

    io.readResult := mem.read(io.readAddr)

    programs.zipWithIndex.foreach {
      case (prog, ind) =>
        val (result, resultValid) = Constructor.processProgram(
          io.valid(ind),
          prog,
          io.arg1, io.arg2, io.arg3
        )

        io.result(ind) := result
        io.ready(ind) := resultValid
    }

  }

  class CircuitTester(dut: DUT, resultsAndSteps: Seq[(BigInt, Int)], arg1: BigInt, arg2: BigInt, arg3: BigInt, memContents: Seq[Int]) extends PeekPokeTester(dut) {
    resultsAndSteps.indices.foreach { ind =>
      poke(dut.io.valid(ind), false)
    }
    step(100) // clear memory

    poke(dut.io.arg1, arg1)
    poke(dut.io.arg2, arg2)
    poke(dut.io.arg3, arg3)

    resultsAndSteps.zipWithIndex.foreach { case ((result, steps), i) =>

      step(100)

      expect(peek(dut.io.ready(i)) == 0, "Circuit should not start on its own")

      poke(dut.io.valid(i), true)

      if (steps > 0) {
        step(steps - 1)
        expect(peek(dut.io.ready(i)) == 0, s"Circuit should not finish its calculations before ${steps}th step")
      } else {
        step(-steps)
      }

      step(1)

      if (steps >= 0) {
        expect(peek(dut.io.ready(i)) == 1, s"Circuit should finish its calculations on ${steps}th step")
      }

      val realResult = peek(dut.io.result(i))
      expect(realResult == result, s"Result should be $result, got $realResult")

      step(10)
      poke(dut.io.valid(i), false)
    }

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

  def testWith(progs: BpfInsn*)(args: (BigInt, BigInt, BigInt), result: BigInt, steps: Int, sparseMemContents: (Int, Int)*): Unit = {
    val mem = sparseMemContents.toMap
    val memContents = (0 until MemLength).map(mem.getOrElse(_, MemInit))
    chisel3.iotesters.Driver(
      () => new DUT(Seq(progs, progs, progs)), backendType="firrtl")(
      new CircuitTester(_, Seq.tabulate(3)(_ => result -> steps), args._1, args._2, args._3, memContents)
    ) shouldBe true
  }

  def compileAndTest(textsAndResults: Seq[(String, BigInt)], args: (BigInt, BigInt), sparseMemContents: (Int, Int)*): Unit = {
    val mem = sparseMemContents.toMap
    val memContents = (0 until MemLength).map(mem.getOrElse(_, MemInit))
    val tempFile = Files.createTempFile("bpf", ".c")
    val fullText =
      s"""#include <stdint.h>
        |volatile uint64_t a, b;
        |uint64_t c, d;
        |uint64_t *addr;
        |""".stripMargin +
        textsAndResults.zipWithIndex.map { case ((text, _), ind) =>
          s"""
            |int f$ind(uint64_t x, uint64_t y) {
            |  $text
            }
          """.stripMargin
        }.mkString
    val results = textsAndResults.map(_._2)
    Files.write(tempFile, fullText.getBytes)
    val fname = tempFile.toAbsolutePath.toString
    Runtime.getRuntime
      .exec(Array("/bin/sh", "-c", s"clang -O3 -emit-llvm -c $fname -o - | llc -march=bpf -filetype=obj -o $fname.o"))
      .waitFor()
    val data = Files.readAllBytes(Paths.get(fname + ".o"))
    val progs = BpfLoader.fetchProgs(ByteBuffer.wrap(data))
    chisel3.iotesters.Driver(
      () => new DUT(progs.map(_.insns)), backendType="firrtl")(
      new CircuitTester(_, results.map(_ -> -100), args._1, args._2, 0, memContents)
    ) shouldBe true
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
    )((1, 2, 3), 0, 3,
      1 -> 0xbc,
      22 -> 3
    )
  }

  it should "load then store" in {
    testWith(
      BpfInsn(0x71, 1, 1, 1,  Left(0)),  // r1 = *(uint8_t *)(r1 + 1)
      BpfInsn(0x07, 1, 0, 0,  Left(3)),  // r1 += 3
      BpfInsn(0x73, 2, 1, 10, Left(0)),  // *(uint8_t *)(r2 + 10) = r1
    )((1, 2, 3), 0, 6,
      12 -> 0xad
    )
  }

  it should "handle byteswap2" in {
    compileAndTest(Seq("return ((x & 0xFF) << 8) | ((x & 0xFF00) >> 8);" -> 0xefcd),
      (0xabcdef, 0x00))
  }

  it should "handle bytemerge2" in {
    compileAndTest(Seq("return (x & 0xFF) | (y & 0xFF00);" -> 0x34ef),
      (0xabcdef, 0x123456))
  }

  it should "handle popcount" in {
    compileAndTest(Seq(
      "return __builtin_popcount(x);" -> 6,
      "return __builtin_popcountl(x);" -> 8),
      (0x10000020AA000011l, 0))
  }

  it should "handle explicit register allocation" in {
    compileAndTest(Seq("a = x; b = a * 2; a = b + 2; return a + b;" -> 6),
      (1, 0))
  }

  it should "store registers between invocations" in {
    compileAndTest(Seq.tabulate(3)(_ => "*(uint8_t*)(a + 1) += ++b; a += x; return 0;" -> 0),
      (3, 0), 1 -> 0xab, 4 -> 0xac, 7 -> 0xad
    )
  }

  it should "store registers between invocations (not volatile)" in {
    compileAndTest(Seq.tabulate(3)(_ => "*(uint8_t*)(c + 1) += ++a; c += x; return 0;" -> 0),
      (3, 0), 1 -> 0xab, 4 -> 0xac, 7 -> 0xad
    )
  }

  it should "handle storing the pointer between funct-s" in {
    compileAndTest(Seq(
      "addr = (void *)x; return ((uint32_t)addr) << 1;" -> 16,
      /* cannot perform not naturally fully ordered operations on a particular byte of memory, so "addr + 1" */
      "*addr = (*addr) + 1; return (*(addr + 1)) & 0xFF;" -> 0xaa,
      "addr += 2; return addr;" -> 24
    ), (8, 0), 8 -> 0xab)
  }
}
