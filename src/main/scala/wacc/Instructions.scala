package wacc

object Instructions {
  sealed trait Operand
    case class Immediate(value: Int) extends Operand
    case class RegOffset(reg: Register, offset: Int) extends Operand
  
  // Need to discuss which pattern here
  sealed abstract class Label(name: String) extends Operand
  
  sealed trait Register extends Operand
    case object R0 extends Register
    case object R1 extends Register
    case object R2 extends Register
    case object R3 extends Register
    case object R4 extends Register
    case object R5 extends Register
    case object R6 extends Register
    case object R7 extends Register
    case object R8 extends Register
    case object R9 extends Register
    case object R10 extends Register
    case object R11 extends Register // Frame Pointer
    case object R12 extends Register
    case object R13 extends Register // Stack Pointer
    case object R14 extends Register // Link Register
    case object R15 extends Register // Program Counter

  /* Aliases for special registers */
  val FP = R11
  val SP = R13
  val LR = R14
  val PC = R15

  sealed trait Instruction

  sealed trait ExprInstr extends Instruction
    case class AddInstr(destReg: Register, reg1: Register, opr: Operand) extends ExprInstr
    case class SubInstr(destReg: Register, reg1: Register, opr: Operand) extends ExprInstr
    case class MulInstr(destRegLo: Register, destRegHi: Register, reg1: Register, reg2: Register) extends ExprInstr
    case class DivInstr(destReg: Register, reg1: Register, reg2: Register) extends ExprInstr // may be unused
    case class ModInstr(destReg: Register, reg1: Register, reg2: Register) extends ExprInstr // may be unused

    case class AndInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr // unused in and, but used in chr
    case class OrInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr // may be unused

    case class CmpInstr(srcReg: Register, opr: Operand) extends ExprInstr

    case class NotInstr(destReg: Register, srcReg: Register) extends ExprInstr
    case class NegInstr(destReg: Register, srcReg: Register) extends ExprInstr
    case class LenInstr(destReg: Register, str: Label) extends ExprInstr
    case class OrdInstr(destReg: Register, imm: Immediate) extends ExprInstr
    case class ChrInstr(destReg: Register, imm: Immediate) extends ExprInstr

    case class RsbsInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr
  
  sealed trait MemoryInstr extends Instruction
    case class StoreInstr(srcReg: Register, destLoc: RegOffset) extends MemoryInstr
    case class LoadInstr(dest: Register, srcLoc: RegOffset) extends MemoryInstr

  sealed trait StatInstr extends Instruction
		case class SkipInstr() extends StatInstr
		case class DeclareInstr() extends StatInstr
		case class AssignInstr() extends StatInstr
		case class IfInstr() extends StatInstr
		case class WhileInstr() extends StatInstr
    case class MovInstr (destReg: Register, opr: Operand) extends StatInstr
    case class CondMovInstr (cond: CondCode, destReg: Register, opr: Operand)

  sealed trait StackInstr extends Instruction
    case class PushInstr(registers: Seq[Register]) extends StackInstr
    case class PopInstr(registers: Seq[Register]) extends StackInstr

  sealed trait JumpInstr extends Instruction
    case class BranchLinkInstr(label: BranchLinkName) extends JumpInstr  // bl exit
    case class BranchInstr(label: Label) extends JumpInstr      // b .L0
    case class CondBranchLinkInstr(cond: CondCode, label: Label) extends JumpInstr
    case class CondBranchInstr(cond: CondCode, label: Label) extends JumpInstr

  sealed trait CondCode
    case object EqCond extends CondCode
    case object NeqCond extends CondCode
    case object GtCond extends CondCode
    case object GteCond extends CondCode
    case object LtCond extends CondCode
    case object LteCond extends CondCode
    
  sealed trait BranchLinkName extends Label
    case object MallocLabel extends Label("malloc") with BranchLinkName
    case object ExitLabel extends Label("exit") with BranchLinkName
    case object FreeLabel extends Label("free") with BranchLinkName

    case object PrintLine extends Label("_println") with BranchLinkName
    case object PrintInt  extends Label("_printi") with BranchLinkName
    case object PrintBool extends Label("_printb") with BranchLinkName
    case object PrintChar extends Label("_printc") with BranchLinkName
    case object PrintStr  extends Label("_prints") with BranchLinkName
    case object PrintPointer extends Label("_printp") with BranchLinkName

    case object ReadInt  extends Label("_readi") with BranchLinkName
    case object ReadChar extends Label("_readc") with BranchLinkName

    case object CheckNull extends Label("_errNull") with BranchLinkName
    case object CheckOverflow extends Label("_errOverflow") with BranchLinkName
    case object CheckDivZero  extends Label("_errDivZero") with BranchLinkName
    case object CheckBound extends Label("_boundsCheck") with BranchLinkName

    case object ArrayStore extends Label("_arrStore") with BranchLinkName
    case object ArrayLoad  extends Label("_arrLoad") with BranchLinkName
    case object FreePair   extends Label("_freepair") with BranchLinkName

    case object DivBl extends Label("__aeabi_idivmod") with BranchLinkName

}
