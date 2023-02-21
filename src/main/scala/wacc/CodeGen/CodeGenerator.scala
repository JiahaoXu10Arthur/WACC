package wacc.CodeGen

import wacc.Instructions._

object CodeGenerator {
  def assemble(instr: Instruction): String = {
    instr match {
      case instr: ExprInstr => assembleExpr(instr)
      case _ => "not implemented yet"
    }
  }

  private def asmReg(reg: Register): String = reg match {
    case R0 => "r0"
    case R1 => "r1"
    case R2 => "r2"
    case R3 => "r3"
    case R4 => "r4"
    case R5 => "r5"
    case R6 => "r6"
    case R7 => "r7"
    case R8 => "r8"
    case R9 => "r9"
    case R10 => "r10"
    case R11 => "fp"
    case R12 => "r12"
    case R13 => "sp"
    case R14 => "lr"
    case R15 => "pc"
  }

  private def asmOp(op: Operand): String = op match {
    case op: Register => asmReg(op)
    case RegOffset(reg, offset) => s"[${asmReg(reg)}, #$offset]"
    case Immediate(value) => s"#$value"
  }

  private def assembleExpr(instr: ExprInstr): String = instr match {
    case AddInstr(destReg, reg1, opr) => {
      val destStr = asmReg(destReg)
      val reg1Str = asmReg(reg1)
      val oprStr = asmOp(opr)
      s"adds $destStr, $reg1Str, $oprStr"
    }
    case SubInstr(destReg, reg1, opr) => {
      val destStr = asmReg(destReg)
      val reg1Str = asmReg(reg1)
      val oprStr = asmOp(opr)
      s"subs $destStr, $reg1Str, $oprStr"
    }
    case MulInstr(destRegLo, destRegHi, reg1, reg2) => {
      val destLoStr = asmReg(destRegLo)
      val destHiStr = asmReg(destRegHi)
      val reg1Str = asmReg(reg1)
      val reg2Str = asmReg(reg2)
      s"smull $destLoStr, $destHiStr, $reg1Str, $reg2Str"
    }
    case AndInstr(destReg, reg1, opr) => {
      val destStr = asmReg(destReg)
      val reg1Str = asmReg(reg1)
      val oprStr = asmOp(opr)
      s"and $destStr, $reg1Str, $oprStr"
    }
    case CmpInstr(reg1, opr) => {
      val reg1Str = asmReg(reg1)
      val oprStr = asmOp(opr)
      s"cmp $reg1Str, $oprStr"
    }
    case _ => "not implemented yet"
  }
}