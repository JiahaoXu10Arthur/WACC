package wacc.CodeGen

import scala.collection.mutable.ListBuffer

import wacc.Instructions._
import wacc.CodeGen.IR
import wacc.CodeGen.IRSegment

object OptimiseIR {
  
  // Optimise push/pop pairs
  def optimisePushPop(seqs: Seq[Instruction]): Seq[Instruction] = {
    val optimisedList = ListBuffer[Instruction]()
    var i = 0

    while (i < seqs.length) {
      seqs(i) match {
        case PushInstr(registers) => {
          if (registers.length == 1 && nextInstrIsPop(seqs(i+1))) {
            val srcReg = registers(0)
            val destReg = getDestReg(seqs(i+1))
            // If move to different register, add mov instruction
            if (srcReg != destReg) {
              optimisedList += MovInstr(destReg, srcReg)
            }
            // Skip the next pop instruction
            i += 1
          } else {
            // If next not pop, add push instruction
            optimisedList += seqs(i)
          }
        }
        case _ => optimisedList += seqs(i)
      }
      // next instruction
      i += 1
    }

    optimisedList.toSeq
  }

  // Optimise MovInstr
  def optimiseMov(seqs: Seq[Instruction]): Seq[Instruction] = {
    val optimisedList = ListBuffer[Instruction]()

    var previousMove: Option[Instruction] = None
    var i = 0

    while (i < seqs.length) {
      var combineMove: Option[Instruction] = None
      val curInstr = seqs(i)
      
      previousMove match {
        case Some(preInstr) => combineMove = connectMove(preInstr, curInstr)
        case None => 
      }
      
      combineMove match {
        case Some(moveInstr) => {
          previousMove = combineMove
        }
        case None => (curInstr, previousMove) match {
          case (MovInstr(_, _), Some(instr)) => {
            optimisedList += instr
            previousMove = Some(curInstr)
          }
          case (MovInstr(_, _), None) => {
            previousMove = Some(curInstr)
          }
          case (_, Some(instr)) => {
            optimisedList += instr
            optimisedList += curInstr
            previousMove = None
          }
          case (_, None) => {
            optimisedList += curInstr
            previousMove = None
          }
        }
      }

      i += 1
    }

    optimisedList.toSeq
  }

  // Optimise store/load pairs
  def optimiseStoreLoad(seqs: Seq[Instruction]): Seq[Instruction] = {
    val optimisedList = ListBuffer[Instruction]()
    var i = 0

    while (i < seqs.length) {
      seqs(i) match {
        case StoreInstr(srcReg, destLocStore, _) => {
          if (nextInstrIsLoad(seqs(i+1))) {
            val destReg = getDestRegLoad(seqs(i+1))
            val srcLocLoad = getSrcLocLoad(seqs(i+1))

            // Adding store instruction
            optimisedList += seqs(i)
            // the load instruction is loading from the same location
            if (destLocStore == srcLocLoad) {
              // If after optimise, move to/from same reg, do not add mov
              // Replacing load with move since move is faster
              optimisedList += MovInstr(destReg, srcReg)

              // Skip the next load instruction
              i += 1
            }
          } else {
            // If next not load, add store instruction
            optimisedList += seqs(i)
          }
        }
        case StoreByteInstr(srcReg, destLocStore, _) => {
          if (nextInstrIsLoadByte(seqs(i+1))) {
            val destReg = getDestRegLoad(seqs(i+1))
            val srcLocLoad = getSrcLocLoad(seqs(i+1))

            // Adding store byte instruction
            optimisedList += seqs(i)
            // the load signed byte instruction is loading from the same location
            if (destLocStore == srcLocLoad) {
              // If after optimise, move to/from same reg, do not add mov
              // Replacing load with move since move is faster
              optimisedList += MovInstr(destReg, srcReg)

              // Skip the next load instruction
              i += 1
            }
          } else {
            // If next not load signed byte, add store byte instruction
            optimisedList += seqs(i)
          }
        }
        case _ => optimisedList += seqs(i)
      }
      // next instruction
      i += 1
    }

    optimisedList.toSeq
  }

  def nextInstrIsPop(instr: Instruction): Boolean = {
    instr match {
      case PopInstr(registers) => (registers.length == 1)
      case _ => false
    }
  }

  def nextInstrIsLoad(instr: Instruction): Boolean = {
    instr match {
      case LoadInstr(destReg, srcLoc, _) => true
      case _ => false
    }
  }

  def nextInstrIsLoadByte(instr: Instruction): Boolean = {
    instr match {
      case LoadSignedByteInstr(destReg, srcLoc, _) => true
      case _ => false
    }
  }

  def getDestReg(instr: Instruction): Register = {
    instr match {
      case PopInstr(registers) => registers(0)
      case _ => null
    }
  }

  def getDestRegLoad(instr: Instruction): Register = {
    instr match {
      case LoadInstr(dest, srcLoc, _) => dest
      case LoadSignedByteInstr(dest, srcLoc, _) => dest
      case _ => null
    }
  }

  def getSrcLocLoad(instr: Instruction): Operand = {
    instr match {
      case LoadInstr(dest, srcLoc, _) => srcLoc
      case LoadSignedByteInstr(dest, srcLoc, _) => srcLoc
      case _ => null
    }
  }

  def connectMove(mov1: Instruction,
                  mov2: Instruction): Option[Instruction] = {
    
    mov1 match {
      case MovInstr(dest1, src1) => {
        // If move to varaible register -> declaration, cannot combine
        if (variableReg.contains(dest1)) {
          return None
        }
      
        mov2 match {
          case MovInstr(dest2, src2) =>
            if (dest1 == src2) {
              Some(MovInstr(dest2, src1))
            } else {
              None
            }
          case _ => None
        }
      }
      case _ => None
    }
  }

  def makeOptimisedIR(originalIR: IR): IR = {
    val mainSeq = originalIR.segments(0).instrs
    val optPushPopSeq = optimisePushPop(mainSeq)
    val optStoreLoadSeq = optimiseStoreLoad(optPushPopSeq)
    val optMovSeq = optimiseMov(optStoreLoadSeq)
    val optimisedMain = new IRSegment(originalIR.segments(0).literals, optMovSeq)
    val irBuilder = ListBuffer[IRSegment]()

    irBuilder += optimisedMain
    originalIR.segments.drop(1).foreach(seg => irBuilder += seg)

    new IR(irBuilder.toSeq)
  }
}
