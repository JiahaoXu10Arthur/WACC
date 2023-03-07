package wacc.CodeGen

import scala.collection.mutable.ListBuffer

import wacc.Instructions._
import wacc.CodeGen.IR
import wacc.CodeGen.IRSegment

object OptimiseIR {
  val optimisedList = ListBuffer[Instruction]()

  def optimise1(ir: IR) = {

    val mainSeq: Seq[Instruction] = ir.segments(0).instrs
    var i = 0

    while (i < mainSeq.length) {
      mainSeq(i) match {
        case PushInstr(registers) => {
          if (registers.length == 1 && nextInstrIsPop(mainSeq(i+1))) {
            optimisedList += MovInstr(registers(0), destReg(mainSeq(i+1)))
            i += 2
          }
        }
        case _ => optimisedList += mainSeq(i)
      }
      i += 1
    }
  }

  def nextInstrIsPop(instr: Instruction): Boolean = {
    instr match {
      case PopInstr(registers) => (registers.length == 1)
      case _ => false
    }
  }

  def destReg(instr: Instruction): Register = {
    instr match {
      case PushInstr(registers) => registers(0)
      case _ => null
    }
  }

  def makeOptimisedIR(originalIR: IR): IR = {
    
    optimise1(originalIR)
    val optimisedMain = new IRSegment(originalIR.segments(0).literals, optimisedList.toSeq)
    val irBuilder = ListBuffer[IRSegment]()

    irBuilder += optimisedMain
    originalIR.segments.drop(1).foreach(seg => irBuilder += seg)

    new IR(irBuilder.toSeq)
  }
}
