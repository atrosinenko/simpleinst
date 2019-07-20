package freechips.rocketchip.instrumenter

import chisel3._

import scala.collection.mutable.ArrayBuffer

class Serializer(isComputing: Bool, next: Bool) {
  def monotonic(x: Bool): Bool = {
    val res = WireInit(false.B)
    val prevRes = RegInit(false.B)
    prevRes := res && isComputing
    res := (x || prevRes) && isComputing
    res
  }
  private def noone(bs: Seq[Bool]): Bool = !bs.foldLeft(false.B)(_ || _)

  private val previousReqs = ArrayBuffer[Bool]()
  def nextReq(x: Bool): (Bool, Int) = {
    val enable = monotonic(x)
    val result = RegInit(false.B)
    val retired = RegInit(false.B)
    val doRetire = result && next
    val thisReq = enable && !retired && !doRetire
    val reqWon = thisReq && noone(previousReqs)
    when (isComputing) {
      when(reqWon) {
        result := true.B
      }
      when(doRetire) {
        result := false.B
        retired := true.B
      }
    } otherwise {
      result := false.B
      retired := false.B
    }
    previousReqs += thisReq
    (result, previousReqs.length - 1)
  }
}
