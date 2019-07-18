package freechips.rocketchip.instrumenter

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.instrumenter.BpfLoader.{BpfInsn, BpfProg}
import freechips.rocketchip.tile.{LazyRoCC, LazyRoCCModuleImp, OpcodeSet}
import freechips.rocketchip.util._

object SimpleInstRoCC {
  def fetchInstructions(bpfObject: String): Map[Int, Seq[BpfInsn]] = {
    val srcs = BpfLoader.fetchProgs(bpfObject)
    srcs.map {
      case BpfProg(name, insns) if name.startsWith("funct") =>
        name.substring(5).toInt -> insns
    }.toMap
  }
}

class SimpleInstRoCC(opcodes: OpcodeSet, val insns: Map[Int, Seq[BpfInsn]])(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module: LazyRoCCModuleImp = new SimpleInstRoCCImp(this)
}

class SimpleInstRoCCImp(outer: SimpleInstRoCC)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  object Constructor extends BpfCircuitConstructor {
    override def doLoad(addr: UInt, lgsize: Int, valid: Bool): (UInt, Bool) = ???
    override def doStore(addr: UInt, lgsize: Int, data: UInt, valid: Bool): Bool = ???
    override def resolveSymbol(sym: BpfLoader.Symbol): UInt = ???
  }

  val cmd = io.cmd
  val funct = cmd.bits.inst.funct

  val isKnown = WireInit(false.B)
  val doneComputing = WireInit(true.B)
  val isComputing = RegInit(false.B)
  val isResponding = RegInit(false.B)

  cmd.ready := !isComputing && !isResponding
  io.resp.valid := isResponding
  io.busy := isComputing || isResponding

  when (cmd.fire()) {
    isComputing := true.B
    isResponding := cmd.bits.inst.xd
  } otherwise {
    when (doneComputing) {
      isComputing := false.B
    }
  }
  when(io.resp.fire()) {
    isResponding := false.B
  }

  io.resp.bits.rd := cmd.bits.inst.rd holdUnless cmd.fire()

  io.mem.req.valid := false.B
  io.interrupt := !isKnown

  outer.insns.foreach {
    case (opcode, insns) =>
      val active = (funct === opcode.U) holdUnless cmd.fire()
      val (result, ready) = Constructor.processProgram(
        (isComputing || cmd.fire()) && active,
        insns,
        cmd.bits.rs1 holdUnless cmd.fire(),
        cmd.bits.rs2 holdUnless cmd.fire()
      )

      when (active) {
        // only when funct is known
        io.resp.bits.data := result
        doneComputing := ready
        isKnown := true.B
      }
  }
}
