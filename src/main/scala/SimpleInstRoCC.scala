package freechips.rocketchip.instrumenter

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.instrumenter.BpfLoader.{BpfInsn, BpfProg}
import freechips.rocketchip.rocket._
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

class SimpleInstRoCC(opcodes: OpcodeSet, val insns: Map[Int, Seq[BpfInsn]], val watchdogTicks: Int = 1000)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module: LazyRoCCModuleImp = new SimpleInstRoCCImp(this)
}

class SimpleInstRoCCImp(outer: SimpleInstRoCC)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  val cmd = io.cmd
  val funct = cmd.bits.inst.funct

  val isKnown = WireInit(false.B)
  val doneComputing = WireInit(true.B)
  val watchdogCountdown = RegInit(0.U(16.W))

  val isComputing = watchdogCountdown =/= 0.U
  watchdogCountdown := Mux(isComputing, watchdogCountdown - 1.U, 0.U)

  val isResponding = RegInit(false.B)

  cmd.ready := !isComputing && !isResponding
  io.resp.valid := isResponding
  io.busy := isComputing || isResponding

  when (cmd.fire()) {
    watchdogCountdown := outer.watchdogTicks.U
    isResponding := cmd.bits.inst.xd
  } otherwise {
    when (doneComputing) {
      watchdogCountdown := 0.U
    }
  }
  when(io.resp.fire()) {
    isResponding := false.B
  }

  io.interrupt := false.B
  io.resp.bits.rd := cmd.bits.inst.rd holdUnless cmd.fire()

  io.mem.req.valid := false.B
  io.mem.req.bits.phys := false.B

  val regs = mutable.Map[String, UInt]()

  class Constructor extends BpfCircuitConstructor {
    val serializer = new Serializer(isComputing, io.mem.req.fire())
    override def doMemLoad(addr: UInt, tpe: LdStType, valid: Bool): (UInt, Bool) = {
      val (doReq, thisTag) = serializer.nextReq(valid)
      when (doReq) {
        io.mem.req.bits.addr := addr
        require((1 << io.mem.req.bits.tag.getWidth) > thisTag)
        io.mem.req.bits.tag := thisTag.U
        io.mem.req.bits.cmd := M_XRD
        io.mem.req.bits.typ := (4 | tpe.lgsize).U
        io.mem.req.bits.data := 0.U
        io.mem.req.valid := true.B
      }

      val doResp = isComputing &&
        serializer.monotonic(doReq && io.mem.req.fire()) &&
        io.mem.resp.valid &&
        io.mem.resp.bits.tag === thisTag.U &&
        io.mem.resp.bits.cmd === M_XRD

      (io.mem.resp.bits.data holdUnless doResp, serializer.monotonic(doResp))
    }
    override def doMemStore(addr: UInt, tpe: LdStType, data: UInt, valid: Bool): Bool = {
      val (doReq, thisTag) = serializer.nextReq(valid)
      when (doReq) {
        io.mem.req.bits.addr := addr
        require((1 << io.mem.req.bits.tag.getWidth) > thisTag)
        io.mem.req.bits.tag := thisTag.U
        io.mem.req.bits.cmd := M_XWR
        io.mem.req.bits.typ := (4 | tpe.lgsize).U
        io.mem.req.bits.data := data
        io.mem.req.valid := true.B
      }
      serializer.monotonic(doReq && io.mem.req.fire())
    }
    override def resolveSymbol(sym: BpfLoader.Symbol): Resolved = sym match {
      case BpfLoader.Symbol(symName, _, size, ElfConstants.Elf64_Shdr.SHN_COMMON, false) if size <= 8 =>
        RegisterReference(regs.getOrElseUpdate(symName, RegInit(0.U(64.W))))
    }
  }

  outer.insns.foreach {
    case (opcode, insns) =>
      val constructor = new Constructor
      val active = (funct === opcode.U) holdUnless cmd.fire()
      val (result, ready) = constructor.processProgram(
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
