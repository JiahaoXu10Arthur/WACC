package wacc

object Instructions {
  sealed trait Operand
    case class Immediate(value: Int) extends Operand
    case class Label(name: String) extends Operand
    case class RegOffset(reg: Register, offset: Int) extends Operand
  
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
    case class AddInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr
    case class SubInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr
    case class MulInstr(destReg: Register, reg1: Register, reg2: Register) extends ExprInstr
    case class DivInstr(destReg: Register, reg1: Register, reg2: Register) extends ExprInstr
    case class ModInstr(destReg: Register, reg1: Register, reg2: Register) extends ExprInstr

    case class AndInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr
    case class OrInstr(destReg: Register, srcReg: Register, opr: Operand) extends ExprInstr
    
    case class CmpGtInstr(srcReg: Register, opr: Operand) extends ExprInstr
    case class CmpGteInstr(srcReg: Register, opr: Operand) extends ExprInstr
    case class CmpLtInstr(srcReg: Register, opr: Operand) extends ExprInstr
    case class CmpLteInstr(srcReg: Register, opr: Operand) extends ExprInstr
    case class CmpEqInstr(srcReg: Register, opr: Operand) extends ExprInstr
    case class CmpNeqInstr(srcReg: Register, opr: Operand) extends ExprInstr

    case class NotInstr(destReg: Register, srcReg: Register) extends ExprInstr
    case class NegInstr(destReg: Register, srcReg: Register) extends ExprInstr
    case class LenInstr(destReg: Register, str: Label) extends ExprInstr
    case class OrdInstr(destReg: Register, imm: Immediate) extends ExprInstr
    case class ChrInstr(destReg: Register, imm: Immediate) extends ExprInstr

  sealed trait StatInstr extends Instruction
		case class SkipInstr() extends StatInstr
		case class DeclareInstr() extends StatInstr
		case class AssignInstr() extends StatInstr
		case class ReadInstr() extends StatInstr
		case class FreeInstr() extends StatInstr
		case class ReturnInstr() extends StatInstr
		case class ExitInstr(opr: Operand) extends StatInstr
		case class PrintInstr() extends StatInstr
		case class PrintlnInstr() extends StatInstr
		case class IfInstr() extends StatInstr
		case class WhileInstr() extends StatInstr
		case class BeginInstr() extends StatInstr
    case class MovInstr (destReg: Register, opr: Operand) extends StatInstr
  
  sealed trait StackInstr extends Instruction
    case class PushInstr(registers: Seq[Register]) extends StackInstr
    case class PopInstr(registers: Seq[Register]) extends StackInstr

  sealed trait JumpInstr extends Instruction
    case class BranchLinkInstr(label: Label) extends JumpInstr  // bl exit
    case class BranchInstr(label: Label)                        // b .L0
  
}
