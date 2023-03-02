package wacc.CodeGen

import java.io._
import wacc.Instructions._
import wacc.CodeGen.IR

object CodeGenerator {
  private final val MovImmMax = 255

  def assemble(ir: IR, fileName: String): String = {
    val asmFile = new File(s"$fileName.s")
    val writer  = new PrintWriter(asmFile)
    /* String constant pool generation */
    writer.println(assembleInstr(DataTag))
    for (str <- ir.strConsts) {
      val asmStrConst = assembleInstr(str)
      writer.println(asmStrConst)
    }
    /* Translate instructions */
    writer.println(assembleInstr(TextTag))
    writer.println(assembleInstr(GlobalTag))
    for (instr <- ir.instrs) {
      val asm = assembleInstr(instr)
      writer.println(asm)
    }

    /* Translate branchlink widgets */
    for (instr <- ir.bLInstrs.flatten) {
        val asm = assembleInstr(instr)
        writer.println(asm)
    }
    writer.close()
    asmFile.createNewFile()
    asmFile.getAbsolutePath()
  }

  def assembleInstr(instr: Instruction): String =
    instr match {
      case instr: ExprInstr   => assembleExpr(instr)
      case instr: JumpInstr   => assembleJump(instr)
      case instr: MemoryInstr => assembleMemory(instr)
      case instr: StackInstr  => assembleStack(instr)
      case instr: StatInstr   => assembleStat(instr)
      case instr: CreateLabel => assembleCreateLabel(instr).mkString("\n")
      case Comment(value)     => s"@$value"
      case instr: Tag         => s".${instr.getName}"
    }


  private def asmLabel(label: Label): String = label match {
    case SegmentLabel(name)    => s".$name"
    case StrLabel(name, value) => s".L.$name"
    case JumpLabel(name)       => s".L$name"
    case label: FuncLabel      => s"${label.getName}"
    case WACCFuncLabel(name)   => s"wacc_$name"
    case _                     => s"@unsupported label creation!"
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
    case op: Register              => asmReg(op)
    case op: Label                 => asmLabel(op)
    case RegIntOffset(reg, offset) => s"[${asmReg(reg)}, #$offset]"
    case RegRegOffset(reg, offset) => s"[${asmReg(reg)}, ${asmReg(offset)}]"
    case RegShiftOffset(reg, offReg, shift) =>
      val regStr    = asmReg(reg)
      val offRegStr = asmReg(offReg)
      val shiftStr  = asmShift(shift)
      s"[$regStr, $offRegStr, $shiftStr]"
    case Immediate(value) => s"#$value"
  }

  private def asmShift(shift: Shifter): String = shift match {
    case LSL(offset) => s"lsl #$offset"
    case ASR(offset) => s"asr #$offset"
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

  private def assembleCreateLabel(instr: CreateLabel): List[String] = instr.label match {
    case StrLabel(name, value) =>
      List(
        s"@ length of ${asmLabel(instr.label)}",
        s".word ${value.length}",
        s"${asmLabel(instr.label)}:",
        s".asciz \"${value.flatMap(unescapeChar(_))}\""
      )
    case SegmentLabel(name) => List(s"${asmLabel(instr.label)}")
    case _                  => List(s"${asmLabel(instr.label)}:")
  }

  private def unescapeChar(c: Char): String = c match {
    case 0x00 => "\\0"
    case 0x08 => "\\b"
    case 0x09 => "\\t"
    case 0x0a => "\\n"
    case 0x0c => "\\f"
    case 0x0d => "\\r"
    case '\\' => "\\"
    case '\"' => "\\\""
    case '\'' => "\\\'"
    case c    => c.toString()
  }

  private def assembleExpr(instr: ExprInstr): String = instr match {
    case AddInstr(destReg, reg1, opr, shifter) =>
      exprAsmGen("adds", destReg, reg1, opr, shifter)
    case SubInstr(destReg, reg1, opr, shifter) =>
      exprAsmGen("subs", destReg, reg1, opr, shifter)
    case RsbsInstr(destReg, srcReg, opr, shifter) =>
      exprAsmGen("rsbs", destReg, srcReg, opr, shifter)
    case MulInstr(destRegLo, destRegHi, reg1, reg2, shifter) =>
      val destLoStr = asmReg(destRegLo)
      val destHiStr = asmReg(destRegHi)
      val reg1Str   = asmReg(reg1)
      val reg2Str   = asmReg(reg2)
      val shiftStr = shifter match {
        case None        => ""
        case Some(shift) => s", ${asmShift(shift)}"
      }
      s"smull $destLoStr, $destHiStr, $reg1Str, $reg2Str$shiftStr"
    case AndInstr(destReg, reg1, opr, shifter) =>
      exprAsmGen("and", destReg, reg1, opr, shifter)
    case CmpInstr(reg1, opr, shifter) =>
      val reg1Str = asmReg(reg1)
      val oprStr  = asmOp(opr)
      val shiftStr = shifter match {
        case None        => ""
        case Some(shift) => s", ${asmShift(shift)}"
      }
      s"cmp $reg1Str, $oprStr$shiftStr"
  }

  // Generates the assembly for an expression instruction with 4 parameters
  private def exprAsmGen(
    name: String,
    destReg: Register,
    srcReg: Register,
    opr: Operand,
    shifter: Option[Shifter]): String = {

    val destStr = asmReg(destReg)
    val srcStr  = asmReg(srcReg)
    val oprStr  = asmOp(opr)
    val shiftStr = shifter match {
      case None        => ""
      case Some(shift) => s", ${asmShift(shift)}"
    }
    s"$name $destStr, $srcStr, $oprStr$shiftStr"
  }

  private def assembleJump(instr: JumpInstr): String = instr match {
    case BranchLinkInstr(label)           => s"bl ${asmLabel(label)}"
    case BranchInstr(label)               => s"b ${asmLabel(label)}"
    case CondBranchLinkInstr(cond, label) => s"bl${asmCond(cond)} ${asmLabel(label)}"
    case CondBranchInstr(cond, label)     => s"b${asmCond(cond)} ${asmLabel(label)}"
  }

  private def assembleMemory(instr: MemoryInstr): String = instr match {
    case StoreInstr(srcReg, destLoc, wb) =>
      memoryAsmGen("str", srcReg, destLoc, wb)
    case StoreByteInstr(srcReg, destLoc, writeBack) =>
      memoryAsmGen("strb", srcReg, destLoc, writeBack)
    case LoadInstr(dest, srcLoc, wb) =>
      memoryAsmGen("ldr", dest, srcLoc, wb)
    case LoadSignedByteInstr(dest, srcLoc, writeBack) =>
      memoryAsmGen("ldrsb", dest, srcLoc, writeBack)
  }

  // Generates the assembly for a memory instruction
  private def memoryAsmGen(
    name: String,
    reg: Register, 
    opr: Operand, 
    writeBack: Boolean): String = {
      val wbStr = if (writeBack) "!" else ""
      s"$name ${asmReg(reg)}, ${asmMemOp(opr)}$wbStr"
  }

  private def asmMemOp(op: Operand): String = op match {
    case op: Register              => asmReg(op)
    case op: Label                 => s"=${asmLabel(op)}"
    case RegIntOffset(reg, offset) => s"[${asmReg(reg)}, #$offset]"
    case RegRegOffset(reg, offset) => s"[${asmReg(reg)}, ${asmReg(offset)}]"
    case RegShiftOffset(reg, offReg, shift) =>
      val regStr    = asmReg(reg)
      val offRegStr = asmReg(offReg)
      val shiftStr  = asmShift(shift)
      s"[$regStr, $offRegStr, $shiftStr]"
    case Immediate(value) => s"=$value"
  }

  private def assembleStat(instr: StatInstr): String = instr match {
    case MovInstr(destReg, opr@Immediate(n)) if n > MovImmMax =>
      s"ldr ${asmReg(destReg)}, ${asmMemOp(opr)}"
    case MovInstr(destReg, opr) => 
      s"mov ${asmReg(destReg)}, ${asmOp(opr)}"
    case CondMovInstr(cond, destReg, opr) =>
      s"mov${asmCond(cond)} ${asmReg(destReg)}, ${asmOp(opr)}"
  }

  private def assembleStack(instr: StackInstr): String = instr match {
    case PopInstr(registers)  => s"pop {${registers.map(asmReg).mkString(", ")}}"
    case PushInstr(registers) => s"push {${registers.map(asmReg).mkString(", ")}}"
  }
}
