package freechips.rocketchip.instrumenter

import chisel3._
import chisel3.util.Cat
import freechips.rocketchip.instrumenter.BpfLoader.BpfInsn

trait BpfCircuitConstructor {
  val mask32 = "hFFFFFFFF".U(64.W)
  val mask64 = "hFFFFFFFFFFFFFFFF".U(64.W)

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

  type LazyData = (UInt, Bool)
  private final case class Context(inputValid: Bool, regMapping: Map[Int, LazyData]) {
    def get(ind: Int): LazyData = regMapping.getOrElse(ind, (0.U(64.W), inputValid))
    def queue(done: Bool): Context = {
      val (r0, r0valid) = get(0)
      set(0, r0 -> (r0valid && done))
    }
    def result: LazyData = get(0)
    def set(ind: Int, data: LazyData): Context = copy(regMapping = regMapping.updated(ind, forceWidth(data._1) -> data._2))
    def resolve(x: Either[Long, BpfLoader.Symbol]): LazyData = x match {
      case Left(value) => value.U -> inputValid
      case Right(sym) => resolveSymbol(sym) -> inputValid
    }
  }

  final case class Opnds(x: UInt, y: UInt, c: UInt) {
    def xy(op: (UInt, UInt) => UInt): UInt = op(x, y)
    def xc(op: (UInt, UInt) => UInt): UInt = op(x, c)
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
      (0.U -% _.x, _.xv),
      (0.U -% _.x, _.xv) // second time
    ),
    constThenY(_ % _), // TODO
    constThenY(_ ^ _),
    Seq[AluOpDesc](
      (_.c, _.cv),
      (_.y, _.yv),
    ),
    constThenY((a, b) => (a.asSInt() >> b(5, 0)).asUInt()),
  ).flatten

  private def createAlu(insn: BpfInsn, tail: List[BpfInsn], ctx: Context, mask: UInt): Context = {
    val num = insn.opcode >> 3
    val (op, opValid) = aluTable(num)

    val (dst, dstValid) = ctx.get(insn.dst)
    val (src, srcValid) = ctx.get(insn.src)
    val (imm, immValid) = ctx.resolve(insn.imm)

    val res = op(Opnds(dst & mask, src & mask, imm & mask))
    val valid = opValid(OpndsValid(dstValid, srcValid, immValid))
    createCircuit(tail, ctx.set(insn.dst, (res, valid)))
  }

  private val loadLgSizeByOpc     = Map(0x61 -> 2, 0x69 -> 1, 0x71 -> 0, 0x79 -> 3)
  private val storeImmLgSizeByOpc = Map(0x62 -> 2, 0x6a -> 1, 0x72 -> 0, 0x7a -> 3)
  private val storeLgSizeByOpc    = Map(0x63 -> 2, 0x69 -> 1, 0x73 -> 0, 0x7b -> 3)

  private def createLdst(insn: BpfInsn, tail: List[BpfInsn], ctx: Context): Context = {
    if (loadLgSizeByOpc.contains(insn.opcode)) {
      val (addr, addrValid) = ctx.get(insn.src)
      val newNode = doLoad(addr + insn.offset.U, loadLgSizeByOpc(insn.opcode), addrValid)
      createCircuit(tail, ctx.set(insn.dst, newNode))
    } else if (storeImmLgSizeByOpc.contains(insn.opcode)) {
      val (addr, addrValid) = ctx.get(insn.dst)
      val (imm, immValid) = ctx.resolve(insn.imm)
      val storeDone = doStore(addr + insn.offset.U, storeImmLgSizeByOpc(insn.opcode), imm, addrValid && immValid)
      createCircuit(tail, ctx.queue(storeDone))
    } else { // store reg
      val (addr, addrValid) = ctx.get(insn.dst)
      val (src , srcValid) = ctx.get(insn.src)
      val storeDone = doStore(addr + insn.offset.U, storeLgSizeByOpc(insn.opcode), src, addrValid && srcValid)
      createCircuit(tail, ctx.queue(storeDone))
    }
  }

  def createByteswap(arg: LazyData, size: Int): LazyData = {
    val (data, valid) = arg
    val resultingBytes = (0 until size / 8).map { byteInd =>
      (data >> (byteInd * 8)).asUInt()(7, 0)
    }
    Cat(resultingBytes) -> valid
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

  def doLoad(addr: UInt, lgsize: Int, valid: Bool): LazyData
  def doStore(addr: UInt, lgsize: Int, data: UInt, valid: Bool): Bool
  def resolveSymbol(sym: BpfLoader.Symbol): UInt

  def processProgram(
                      valid: Bool,
                      program: Seq[BpfInsn],
                      args: UInt*
                    ): LazyData = {
    val ctx = Context(
      valid,
      args.zipWithIndex.map {
        case (arg, ind) => (ind + 1) -> (forceWidth(arg & mask64) -> valid)
      }.toMap,
    )
    createCircuit(program.toList, ctx).result
  }
}
