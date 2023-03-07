package wacc.CodeGen

import scala.collection.mutable.ListBuffer

import wacc.Instructions._
import wacc.CodeGen.IR
import wacc.CodeGen.IRSegment

object OptimiseIR {
  
  def optimise1(ir: IR): Seq[Instruction] = {
    // Optimise push/pop pairs
    val mainSeq: Seq[Instruction] = ir.segments(0).instrs
    val optimisedList = ListBuffer[Instruction]()
    var i = 0

    while (i < mainSeq.length) {
      mainSeq(i) match {
        case PushInstr(registers) => {
          if (registers.length == 1 && nextInstrIsPop(mainSeq(i+1))) {
            val srcReg = registers(0)
            val destReg = getDestReg(mainSeq(i+1))
            // If move to different register, add mov instruction
            if (srcReg != destReg) {
              optimisedList += MovInstr(destReg, srcReg)
            }
            // Skip the next pop instruction
            i += 1
          } else {
            // If next not pop, add push instruction
            optimisedList += mainSeq(i)
          }
        }
        case _ => optimisedList += mainSeq(i)
      }
      // next instruction
      i += 1
    }

    optimisedList.toSeq
  }

  // def optimise2(ir: IR) = {
  //   // Optimise MovInstr

  // }

  def nextInstrIsPop(instr: Instruction): Boolean = {
    instr match {
      case PopInstr(registers) => (registers.length == 1)
      case _ => false
    }
  }

  def getDestReg(instr: Instruction): Register = {
    instr match {
      case PopInstr(registers) => registers(0)
      case _ => null
    }
  }

  def makeOptimisedIR(originalIR: IR): IR = {
    val mainInstrSeq = optimise1(originalIR)
    val optimisedMain = new IRSegment(originalIR.segments(0).literals, mainInstrSeq)
    val irBuilder = ListBuffer[IRSegment]()

    irBuilder += optimisedMain
    originalIR.segments.drop(1).foreach(seg => irBuilder += seg)

    new IR(irBuilder.toSeq)
  }
}
