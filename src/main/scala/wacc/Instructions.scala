package wacc

object Instructions {

  sealed trait Operand
  case class Immediate(value: Int) extends Operand
  sealed class Label(name: String) extends Operand {
    def getName: String = name
  }

  case class SegmentLabel(name: String)            extends Label(name)
  case class StrLabel(name: String, value: String) extends Label(name)
  case class JumpLabel(name: String)               extends Label(name)
  case class WACCFuncLabel(name: String)           extends Label(name)
  case class WACCFuncBodyLabel(fname: String)      extends Label(fname)

  sealed trait Location                                                      extends Operand
  case class RegIntOffset(reg: Register, offset: Int)                        extends Location
  case class RegRegOffset(reg: Register, offset: Register)                   extends Location
  case class RegShiftOffset(reg: Register, offReg: Register, shift: Shifter) extends Location

  sealed trait Shifter
  case class LSL(shift: Int) extends Shifter
  case class ASR(shift: Int) extends Shifter

  sealed trait Register extends Location
  case object R0        extends Register
  case object R1        extends Register
  case object R2        extends Register
  case object R3        extends Register
  case object R4        extends Register
  case object R5        extends Register
  case object R6        extends Register
  case object R7        extends Register
  case object R8        extends Register
  case object R9        extends Register
  case object R10       extends Register
  case object R11       extends Register // Frame Pointer
  case object R12       extends Register // Secondary Frame Pointer
  case object R13       extends Register // Stack Pointer
  case object R14       extends Register // Link Register
  case object R15       extends Register // Program Counter

  /* Aliases for special registers */
  val FP  = R11
  val SFP = R12
  val SP  = R13
  val LR  = R14
  val PC  = R15

  /* Value storage register */
  val OpRet = R0 // For branchLink convention
  val OpR1  = R8
  val OpR2  = R9

  /* Parameter storage register */
  val Param1 = R0
  val Param2 = R1
  val Param3 = R2

  /* Array/Pair registers */
  val MPtr = R12 // Memory allocation pointer

  val CPtr = R7  // Class this. pointer

  val paramReg    = List(R0, R1, R2)
  val variableReg = List(R4, R5, R6)
  val reservedReg = List(R8, R10, R12)

  /* Constant */
  final val TrueImm            = Immediate(1)
  final val FalseImm           = Immediate(0)
  final val NullImm            = Immediate(0)
  final val ChrImm             = Immediate(127)
  final val MulShiftConst      = ASR(31)
  final val FFlushStreamImm    = Immediate(0)
  final val DefaultExitCodeImm = Immediate(0)
  final val ErrorExitCode      = Immediate(255)

  final val ByteSize    = 1
  final val PtrSize     = 4
  final val DefaultSize = 4
  final val PairSize    = 8
  final val PairFstIdx  = 0
  final val PairSndIdx  = 4

  final val ArrayLenOffset = -4
  final val MovImmMax      = 255

  final val SelfPtrStr = "this"

  sealed trait Instruction

  sealed trait ExprInstr extends Instruction
  case class AddInstr(
      destReg: Register,
      reg1: Register,
      opr: Operand,
      shifter: Option[Shifter] = None
  ) extends ExprInstr
  case class SubInstr(
      destReg: Register,
      reg1: Register,
      opr: Operand,
      shifter: Option[Shifter] = None
  ) extends ExprInstr
  case class MulInstr(
      destRegLo: Register,
      destRegHi: Register,
      reg1: Register,
      reg2: Register,
      shifter: Option[Shifter] = None
  ) extends ExprInstr
  case class RsbsInstr(
      destReg: Register,
      srcReg: Register,
      opr: Operand,
      shifter: Option[Shifter] = None
  ) extends ExprInstr
  case class AndInstr(
      destReg: Register,
      srcReg: Register,
      opr: Operand,
      shifter: Option[Shifter] = None
  ) extends ExprInstr // bitwise and instruction
  case class CmpInstr(srcReg: Register, opr: Operand, shifter: Option[Shifter] = None) extends ExprInstr

  sealed trait MemoryInstr                                                                    extends Instruction
  case class StoreInstr(srcReg: Register, destLoc: Operand, writeBack: Boolean = false)       extends MemoryInstr
  case class StoreByteInstr(srcReg: Register, destLoc: Operand, writeBack: Boolean = false)   extends MemoryInstr
  case class LoadInstr(dest: Register, srcLoc: Operand, writeBack: Boolean = false)           extends MemoryInstr
  case class LoadSignedByteInstr(dest: Register, srcLoc: Operand, writeBack: Boolean = false) extends MemoryInstr

  sealed trait StatInstr                                                   extends Instruction
  case class MovInstr(destReg: Register, opr: Operand)                     extends StatInstr
  case class CondMovInstr(cond: CondCode, destReg: Register, opr: Operand) extends StatInstr

  sealed trait StackInstr                        extends Instruction
  case class PushInstr(registers: Seq[Register]) extends StackInstr
  case class PopInstr(registers: Seq[Register])  extends StackInstr

  sealed trait JumpInstr                                       extends Instruction
  case class BranchLinkInstr(label: Label)                     extends JumpInstr // bl exit
  case class BranchInstr(label: Label)                         extends JumpInstr // b .L0
  case class CondBranchLinkInstr(cond: CondCode, label: Label) extends JumpInstr
  case class CondBranchInstr(cond: CondCode, label: Label)     extends JumpInstr

  case class CreateLabel(label: Label) extends Instruction
  case class Comment(value: String)    extends Instruction

  sealed class Tag(name: String) extends Instruction {
    def getName: String = name
  }

  case object DataTag   extends Tag("data")
  case object TextTag   extends Tag("text")
  case object GlobalTag extends Tag("global main")
  case object LtorgTag  extends Tag("ltorg")

  sealed trait CondCode
  case object EqCond  extends CondCode
  case object NeqCond extends CondCode
  case object GtCond  extends CondCode
  case object GteCond extends CondCode
  case object LtCond  extends CondCode
  case object LteCond extends CondCode
  case object VsCond  extends CondCode

  sealed trait FuncLabel extends Label

  case object PrintLine    extends Label("_println") with FuncLabel
  case object PrintInt     extends Label("_printi") with FuncLabel
  case object PrintBool    extends Label("_printb") with FuncLabel
  case object PrintChar    extends Label("_printc") with FuncLabel
  case object PrintStr     extends Label("_prints") with FuncLabel
  case object PrintPointer extends Label("_printp") with FuncLabel

  case object ReadInt  extends Label("_readi") with FuncLabel
  case object ReadChar extends Label("_readc") with FuncLabel

  case object CheckNull     extends Label("_errNull") with FuncLabel
  case object CheckOverflow extends Label("_errOverflow") with FuncLabel
  case object CheckDivZero  extends Label("_errDivZero") with FuncLabel
  case object CheckBound    extends Label("_boundsCheck") with FuncLabel

  case object ArrayStore  extends Label("_arrStore") with FuncLabel
  case object ArrayStoreB extends Label("_arrStoreB") with FuncLabel
  case object ArrayLoad   extends Label("_arrLoad") with FuncLabel
  case object ArrayLoadB  extends Label("_arrLoadB") with FuncLabel
  case object FreePair    extends Label("_freepair") with FuncLabel

  /* standard library functions */
  case object DivisionLabel  extends Label("__aeabi_idivmod") with FuncLabel
  case object MallocLabel    extends Label("malloc") with FuncLabel
  case object ExitLabel      extends Label("exit") with FuncLabel
  case object FreeLabel      extends Label("free") with FuncLabel
  case object PrintFormatted extends Label("printf") with FuncLabel
  case object ScanFormatted  extends Label("scanf") with FuncLabel
  case object FileFlush      extends Label("fflush") with FuncLabel
  case object Puts           extends Label("puts") with FuncLabel

  /* Main function */
  case object Main extends Label("main") with FuncLabel
}
