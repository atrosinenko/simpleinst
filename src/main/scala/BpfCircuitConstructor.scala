package freechips.rocketchip.instrumenter

import chisel3._
import chisel3.util.Cat
import freechips.rocketchip.instrumenter.BpfLoader.BpfInsn
import freechips.rocketchip.util._

trait BpfCircuitConstructor {
  val mask32 = "hFFFFFFFF".U(64.W)
  val mask64 = "hFFFFFFFFFFFFFFFF".U(64.W)

  def posedge(x: Bool): Bool = x && !RegNext(x)

  def forceWidth(data: UInt, width: Int = 64): UInt = {
    require(data.isWidthKnown)
    val w = data.getWidth
    if (w < width)
      Cat(0.U(width - w), data)
    else if (w > width)
      data(width - 1, 0)
    else
      data
  }

  type LazyData = (Resolved, Bool)
  private final case class Context
  (
    inputValid: Bool,
    globalReady: Bool,
    regMapping: Map[Int, LazyData],
    auxRegMapping: Map[UInt, Bool] = Map.empty
  ) {
    def getAuxValid(reg: UInt): Bool = auxRegMapping.getOrElse(reg, inputValid)
    def withAuxValid(reg: UInt, valid: Bool): Context = copy(
      auxRegMapping = auxRegMapping.updated(reg, getAuxValid(reg) && valid)
    )

    def get(ind: Int): LazyData = regMapping.getOrElse(ind, (Value(forceWidth(0.U)), inputValid))
    def queue(maybeReg: Resolved, done: Bool): Context = {
      val newGlobalReady = globalReady && done
      val t = maybeReg match {
        case Value(_) => this
        case RegisterReference(reg) => withAuxValid(reg, done)
      }
      t.copy(globalReady = newGlobalReady)
    }
    def result: (UInt, Bool) = {
      val (Value(result), resultValid) = get(0)
      result -> (resultValid && globalReady)
    }
    def set(ind: Int, data: LazyData): Context = copy(regMapping = regMapping.updated(ind, data))
    def resolve(x: Either[Long, BpfLoader.Symbol]): (Resolved, Bool) = x match {
      case Left(value) => Value(value.U) -> inputValid
      case Right(sym) => resolveSymbol(sym) -> inputValid
    }
  }

  final case class Opnds(x: () => UInt, y: () => UInt, c: () => UInt) {
    def xy(op: (UInt, UInt) => UInt): UInt = op(x(), y())
    def xc(op: (UInt, UInt) => UInt): UInt = op(x(), c())
  }
  final case class OpndsValid(xv: Bool, yv: Bool, cv: Bool) {
    def xy: Bool = xv && yv
    def xc: Bool = xv && cv
  }
  type AluOpDesc = (Opnds => UInt, OpndsValid => Bool)
  private def constThenY(op: (UInt, UInt) => UInt) = Seq[AluOpDesc](
    (_.xc(op), _.xc),
    (_.xy(op), _.xy)
  )
  private def aluTable: Seq[AluOpDesc] = Seq(
    constThenY(_ +% _),
    constThenY(_ -% _),
    constThenY(_ * _),
    constThenY(_ / _),
    constThenY(_ | _),
    constThenY(_ & _),
    constThenY((a, b) => (a << b(5, 0)).asUInt()),
    constThenY((a, b) => (a >> b(5, 0)).asUInt()),
    Seq[AluOpDesc](
      (0.U -% _.x(), _.xv),
      (0.U -% _.x(), _.xv) // second time
    ),
    constThenY(_ % _), // TODO
    constThenY(_ ^ _),
    Seq[AluOpDesc](
      (_.c(), _.cv),
      (_.y(), _.yv),
    ),
    constThenY((a, b) => (a.asSInt() >> b(5, 0)).asUInt()),
  ).flatten

  private def createAlu(insn: BpfInsn, tail: List[BpfInsn], ctx: Context, mask: UInt): Context = {
    val num = insn.opcode >> 3
    val (op, opValid) = aluTable(num)

    val (dst, dstValid) = ctx.get(insn.dst)
    val (src, srcValid) = ctx.get(insn.src)
    val (imm, immValid) = ctx.resolve(insn.imm)

    val res = op(Opnds(
      () => dst.asPlainValue & mask,
      () => src.asPlainValue & mask,
      () => imm.asPlainValue & mask
    ))
    val valid = opValid(OpndsValid(dstValid, srcValid, immValid))
    createCircuit(tail, ctx.set(insn.dst, (Value(forceWidth(res)), valid)))
  }

  private val loadLgSizeByOpc     = Map(0x61 -> u32, 0x69 -> u16, 0x71 -> u8, 0x79 -> u64)
  private val storeImmLgSizeByOpc = Map(0x62 -> u32, 0x6a -> u16, 0x72 -> u8, 0x7a -> u64)
  private val storeLgSizeByOpc    = Map(0x63 -> u32, 0x69 -> u16, 0x73 -> u8, 0x7b -> u64)

  private def createLdst(insn: BpfInsn, tail: List[BpfInsn], ctx: Context): Context = {
    if (loadLgSizeByOpc.contains(insn.opcode)) {
      val (addr, addrValid) = ctx.get(insn.src)
      val newNode = addr.load(ctx, insn.offset, loadLgSizeByOpc(insn.opcode), addrValid)
      createCircuit(tail, ctx.queue(addr, newNode._2).set(insn.dst, newNode))
    } else if (storeImmLgSizeByOpc.contains(insn.opcode)) {
      val (addr, addrValid) = ctx.get(insn.dst)
      val (Value(imm), immValid) = ctx.resolve(insn.imm)
      val storeDone = addr.store(insn.offset, storeImmLgSizeByOpc(insn.opcode), imm, addrValid && immValid)
      createCircuit(tail, ctx.queue(addr, storeDone))
    } else { // store reg
      val (addr, addrValid) = ctx.get(insn.dst)
      val (Value(src), srcValid) = ctx.get(insn.src)
      val storeDone = addr.store(insn.offset, storeLgSizeByOpc(insn.opcode), src, addrValid && srcValid)
      createCircuit(tail, ctx.queue(addr, storeDone))
    }
  }

  def createByteswap(arg: LazyData, size: Int): LazyData = {
    val (Value(data), valid) = arg
    val resultingBytes = (0 until size / 8).map { byteInd =>
      (data >> (byteInd * 8)).asUInt()(7, 0)
    }
    Value(Cat(resultingBytes)) -> valid
  }

  // TODO add sequential dependencies?
  private def createCircuit(progTail: List[BpfInsn], ctx: Context): Context = {
    progTail match {
      case Nil => ctx
      case i :: Nil if i.opcode == 0x95 => ctx // exit
      case i :: is if i.opcode == 0xd4 => // htole on le host
        createCircuit(is, ctx)
      case i :: is if i.opcode == 0xdc => // htobe on le host
        createCircuit(is, ctx.set(i.dst, createByteswap(ctx.get(i.dst), i.imm.left.get.toInt)))
      case i :: is if (i.opcode & 0x07) == 0x07 => // 64 bit ALU
        createAlu(i, is, ctx, mask64)
      case i :: is if (i.opcode & 0x07) == 0x04 => // 32 bit ALU
        createAlu(i, is, ctx, mask32)
      case i :: is if i.opcode == 0x18 => // load imm
        createCircuit(is, ctx.set(i.dst, ctx.resolve(i.imm)))
      case i :: is if Seq(0x01, 0x02, 0x03).contains(i.opcode & 0x07) =>
        createLdst(i, is, ctx)
      case _ => ???
    }
  }

  sealed abstract class LdStType(val lgsize: Int) {
    val byteSize = 1 << lgsize
    val bitSize = byteSize * 8
    val mask: UInt = if (bitSize == 64) mask64 else ((1l << bitSize) - 1).U
  }
  case object u8  extends LdStType(0)
  case object u16 extends LdStType(1)
  case object u32 extends LdStType(2)
  case object u64 extends LdStType(3)

  def doMemLoad(addr: UInt, tpe: LdStType, valid: Bool): (UInt, Bool)
  def doMemStore(addr: UInt, tpe: LdStType, data: UInt, valid: Bool): Bool

  sealed trait Resolved {
    def asPlainValue: UInt
    def load(ctx: Context, offset: Int, tpe: LdStType, valid: Bool): LazyData
    def store(offset: Int, tpe: LdStType, data: UInt, valid: Bool): Bool
  }
  final case class Value(value: UInt) extends Resolved {
    override def asPlainValue: UInt = value
    override def load(ctx: Context, offset: Int, tpe: LdStType, valid: Bool): (Resolved, Bool) = {
      val (result, done) = doMemLoad(value + offset.U, tpe, valid)
      Value(result) -> done
    }
    override def store(offset: Int, tpe: LdStType, data: UInt, valid: Bool): Bool = {
      doMemStore(value + offset.U, tpe, data, valid)
    }
  }
  final case class RegisterReference(reg: UInt) extends Resolved {
    override def asPlainValue: UInt = ???
    override def load(ctx: Context, offset: Int, tpe: LdStType, valid: Bool): (Resolved, Bool) = {
      require(offset == 0)
      val allValid = valid && ctx.getAuxValid(reg)
      when (posedge(allValid)) {
        printf("Reg load: %d\n", reg & tpe.mask)
      }
      Value((reg & tpe.mask) holdUnless posedge(allValid)) -> allValid
    }

    override def store(offset: Int, tpe: LdStType, data: UInt, valid: Bool): Bool = {
      require(offset == 0)
      when (posedge(valid)) {
        printf("Reg store: %d\n", data & tpe.mask)
        reg := data & tpe.mask
      }
      valid && RegNext(valid)
    }
  }
  def resolveSymbol(sym: BpfLoader.Symbol): Resolved

  def processProgram(
                      valid: Bool,
                      program: Seq[BpfInsn],
                      args: UInt*
                    ): (UInt, Bool) = {
    val ctx = Context(
      valid,
      valid,
      args.zipWithIndex.map {
        case (arg, ind) => (ind + 1) -> (Value(forceWidth(arg & mask64)) -> valid)
      }.toMap,
    )
    createCircuit(program.toList, ctx).result
  }
}
