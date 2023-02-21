package wacc.CodeGen

import java.io._
import wacc.Instructions._

object CodeGenerator {
  def assemble(instrs: Seq[Instruction], fileName: String): Unit = {
    val asmFile = new File(s"$fileName.s")
    val writer = new PrintWriter(asmFile)
    writer.println(".data")
    writer.println(".text")
    writer.println(".global main")
    for (instr <- instrs) {
      val asm = assembleInstr(instr)
      writer.println(asm)
    }
    writer.close()
    asmFile.createNewFile()
  }

  def assembleInstr(instr: Instruction): String = {
    instr match {
      case instr: ExprInstr   => assembleExpr(instr)
      case instr: JumpInstr   => assembleJump(instr)
      case instr: MemoryInstr => assembleMemory(instr)
      case instr: StackInstr  => assembleStack(instr)
      case instr: StatInstr   => assembleStat(instr)
      case CreateLabel(label)  => s"${label.getName}:"
      case _                  => "not implemented yet"
    }
  }

  private def asmReg(reg: Register): String = reg match {
    case R0  => "r0"
    case R1  => "r1"
    case R2  => "r2"
    case R3  => "r3"
    case R4  => "r4"
    case R5  => "r5"
    case R6  => "r6"
    case R7  => "r7"
    case R8  => "r8"
    case R9  => "r9"
    case R10 => "r10"
    case R11 => "fp"
    case R12 => "r12"
    case R13 => "sp"
    case R14 => "lr"
    case R15 => "pc"
  }

  private def asmOp(op: Operand): String = op match {
    case op: Register           => asmReg(op)
    case op: Label              => op.getName
    case RegOffset(reg, offset) => s"[${asmReg(reg)}, #$offset]"
    case Immediate(value)       => s"#$value"
  }

  private def asmCond(cond: CondCode): String = cond match {
    case EqCond  => "eq"
    case GtCond  => "gt"
    case GteCond => "ge"
    case LtCond  => "lt"
    case LteCond => "le"
    case VsCond  => "vs"
    case NeqCond => "ne"
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
    case RsbsInstr(destReg, srcReg, opr) => {
      val destStr = asmReg(destReg)
      val srcStr = asmReg(srcReg)
      val oprStr = asmOp(opr)
      s"rsbs $destStr, $srcStr, $oprStr"
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

  private def assembleJump(instr: JumpInstr): String = instr match {
    case BranchLinkInstr(label)           => s"bl ${label.getName}"
    case BranchInstr(label)               => s"b ${label.getName}"
    case CondBranchLinkInstr(cond, label) => s"bl${asmCond(cond)} ${label.getName}"
    case CondBranchInstr(cond, label)     => s"b${asmCond(cond)} ${label.getName}"
  }

  private def assembleMemory(instr: MemoryInstr): String = instr match {
    case StoreInstr(srcReg, destLoc) => s"str ${asmReg(srcReg)}, ${asmOp(destLoc)}"
    case LoadInstr(dest, srcLoc)     => s"ldr ${asmReg(dest)}, ${asmOp(srcLoc)}"
  }

  private def assembleStat(instr: StatInstr): String = instr match {
    case MovInstr(destReg, opr)           => s"mov ${asmReg(destReg)}, ${asmOp(opr)}"
    case CondMovInstr(cond, destReg, opr) => s"mov${asmCond(cond)}, ${asmReg(destReg)}, ${asmOp(opr)}"
  }

  private def assembleStack(instr: StackInstr): String = instr match {
    case PopInstr(registers)  => s"pop {${registers.map(asmReg).mkString(", ")}}"
    case PushInstr(registers) => s"push {${registers.map(asmReg).mkString(", ")}}"
  }
}
