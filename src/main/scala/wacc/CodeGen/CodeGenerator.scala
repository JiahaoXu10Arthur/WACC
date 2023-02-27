package wacc.CodeGen

import java.io._
import wacc.Instructions._
import wacc.CodeGen.IR

object CodeGenerator {
  def assemble(ir: IR, fileName: String): String = {
    val asmFile = new File(s"$fileName.s")
    val writer = new PrintWriter(asmFile)
    /* String constant pool generation */
    writer.println(".data")
    for ((str, i) <- ir.strConsts.zipWithIndex) {
      writer.println(s"@ length of .L.str$i")
      writer.println(s".word ${str.length}")
      writer.println(s".L.str$i:")
      writer.println(s".asciz \"${str}\"")
    }
    writer.println(".text")
    /* Translate instructions */
    writer.println(".global main")
    for (instr <- ir.instrs) {
      val asm = assembleInstr(instr)
      writer.println(asm)
    }

    /* Translate branchlink widgets */
    for (instrs <- ir.bLInstrs) {
      for (instr <- instrs) {
        val asm = assembleInstr(instr)
        writer.println(asm)
      }
    }
    
    writer.close()
    asmFile.createNewFile()
    asmFile.getAbsolutePath()
  }

  def assembleInstr(instr: Instruction): String = {
    instr match {
      case instr: ExprInstr   => assembleExpr(instr)
      case instr: JumpInstr   => assembleJump(instr)
      case instr: MemoryInstr => assembleMemory(instr)
      case instr: StackInstr  => assembleStack(instr)
      case instr: StatInstr   => assembleStat(instr)
      case CreateLabel(label) => asmLabel(label)
      case _                  => "@not implemented yet!"
    }
  }

  private def asmLabel(label: Label): String = label match {
    case StrLabel(name, value) => s".L.$name"
    case JumpLabel(name)       => s".$name"
    case label: FuncLabel      => s"${label.getName}:"
    case WACCFuncLabel(name)   => s"wacc_$name:"
    case _                     => s"@unsupported label creation"
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
    case op: Register                  => asmReg(op)
    case op: Label                     => asmLabel(op)
    case RegIntOffset(reg, offset)     => s"[${asmReg(reg)}, #$offset]"
    case RegRegOffset(reg, offset) => s"[${asmReg(reg)}, ${asmReg(offset)}]"
    case RegShiftOffset(reg, offReg, shift) => {
      val regStr = asmReg(reg)
      val offRegStr = asmReg(offReg)
      val shiftStr = asmShift(shift)
      s"[$regStr, $offRegStr, $shiftStr]"
    }
    case Immediate(value) if value < 0 => s"=$value"
    case Immediate(value)              => s"#$value"
  }

  private def asmShift(shift: Shifter): String = shift match {
    case LSL(offset) => s"lsl #$offset"
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
    case StoreInstr(srcReg, destLoc, wb) => {
      val wbStr = if (wb) "!" else ""
      s"str ${asmReg(srcReg)}, ${asmOp(destLoc)}$wbStr"
    }
    case StoreByteInstr(srcReg, destLoc, writeBack) => {
      val wbStr = if (writeBack) "!" else ""
      s"strb ${asmReg(srcReg)}, ${asmOp(destLoc)}$wbStr"
    }
    case LoadInstr(dest, srcLoc, wb) => {
      val wbStr = if (wb) "!" else ""
      s"ldr ${asmReg(dest)}, ${asmOp(srcLoc)}$wbStr"
    }
    case LoadSignedByteInstr(dest, srcLoc, writeBack) => {
      val wbStr = if (writeBack) "!" else ""
      s"ldrsb ${asmReg(dest)}, ${asmOp(srcLoc)}$wbStr"
    }
  }

  private def assembleStat(instr: StatInstr): String = instr match {
    case MovInstr(destReg, opr) => s"mov ${asmReg(destReg)}, ${asmOp(opr)}"
    case CondMovInstr(cond, destReg, opr) =>
      s"mov${asmCond(cond)}, ${asmReg(destReg)}, ${asmOp(opr)}"
  }

  private def assembleStack(instr: StackInstr): String = instr match {
    case PopInstr(registers)  => s"pop {${registers.map(asmReg).mkString(", ")}}"
    case PushInstr(registers) => s"push {${registers.map(asmReg).mkString(", ")}}"
  }
}
