package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._
import wacc.CodeGen.IR.addBranchLink

object BranchLinkTranslator {
  final private val ErrorExitCode  = 255
  final private val ArrayLenOffset = -4

  def translateBranchLink(blName: FuncLabel)(implicit ir: IR): List[Instruction] = {
    implicit val instrsBuffer = mutable.ListBuffer[Instruction]()

    blName match {
      case ArrayLoad     => translateArrayLoad(blName)
      case ArrayLoadB     => translateArrayLoad(blName, loadByte = true)
      case ArrayStore    => translateArrayStore(blName)
      case ArrayStoreB   => translateArrayStore(blName, storeByte = true)
      case CheckBound    => translateCheckBound(blName)
      case CheckDivZero  => translateCheckDivZero(blName)
      case CheckNull     => translateCheckNull(blName)
      case CheckOverflow => translateCheckOverflow(blName)
      case FreePair      => translateFreePair(blName)
      case PrintBool     => translatePrintBool(blName)
      case PrintChar     => translatePrintChar(blName)
      case PrintInt      => translatePrintInt(blName)
      case PrintLine     => translatePrintLine(blName)
      case PrintPointer  => translatePrintPointer(blName)
      case PrintStr      => translatePrintStr(blName)
      case ReadChar      => translateReadChar(blName)
      case ReadInt       => translateReadInt(blName)
      case _             =>
    }

    instrsBuffer.toList
  }

  /*
	Special calling convention: array ptr passed in R3, index in R10,
	LR (R14) is used as general register, and return into R3
   */
  private def translateArrayLoad(
      blName: FuncLabel,
      loadByte: Boolean = false
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction], ir: IR): Unit = {
    lableCreation("", blName, true)
    instrsBuffer += CmpInstr(R10, Immediate(0))
    instrsBuffer += CondMovInstr(LtCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(LtCond, CheckBound)
    instrsBuffer += LoadInstr(LR, RegIntOffset(R3, ArrayLenOffset))
    instrsBuffer += CmpInstr(R10, LR)
    instrsBuffer += CondMovInstr(GteCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(GteCond, CheckBound)
    instrsBuffer += (
      if (loadByte)
        LoadSignedByteInstr(R3, RegRegOffset(R3, R10))
      else
        LoadInstr(R3, RegShiftOffset(R3, R10, LSL(2)))
    )
    instrsBuffer += PopInstr(Seq(PC))

    /* Since translation uses CheckBound, we translate CheckBound as well */
    addBranchLink(CheckBound)
  }

  /*
	Special calling convention: array ptr passed in R3, index in R10,
	value to store in R8, LR (R14) is used as general register
   */
  private def translateArrayStore(
      blName: FuncLabel,
      storeByte: Boolean = false
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction], ir: IR): Unit = {
    lableCreation("", blName, true)
    instrsBuffer += CmpInstr(R10, Immediate(0))
    instrsBuffer += CondMovInstr(LtCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(LtCond, CheckBound)
    instrsBuffer += LoadInstr(LR, RegIntOffset(R3, ArrayLenOffset))
    instrsBuffer += CmpInstr(R10, LR)
    instrsBuffer += CondMovInstr(GteCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(GteCond, CheckBound)
    instrsBuffer += (
      if (storeByte)
        StoreByteInstr(R8, RegRegOffset(R3, R10))
      else
        StoreInstr(R8, RegShiftOffset(R3, R10, LSL(2)))
    )
    instrsBuffer += PopInstr(Seq(PC))

    /* Since translation uses CheckBound, we translate CheckBound as well */
    addBranchLink(CheckBound)
  }

  private def translateFreePair(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction], ir: IR): Unit = {
    lableCreation("", blName, true)
    instrsBuffer += MovInstr(R8, R0)
    /* Pair null check */
    instrsBuffer += CmpInstr(R8, Immediate(0))
    instrsBuffer += CondBranchLinkInstr(EqCond, CheckNull)

    /* Free pair elements */
    val fstElemOffset = 0
    val sndElemOffset = 4
    instrsBuffer += LoadInstr(R0, RegIntOffset(R8, fstElemOffset))
    instrsBuffer += BranchLinkInstr(FreeLabel)
    instrsBuffer += LoadInstr(R0, RegIntOffset(R8, sndElemOffset))
    instrsBuffer += BranchLinkInstr(FreeLabel)

    /* Free pair */
    instrsBuffer += MovInstr(R0, R8)
    instrsBuffer += BranchLinkInstr(FreeLabel)
    instrsBuffer += PopInstr(Seq(PC))

    /* Since translation uses CheckNull, we translate CheckNull as well */
    addBranchLink(CheckNull)
  }

  private def translateCheckBound(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: array index %d out of bounds\n")

    errorCheckCreation(blName, errorStr, PrintFormatted, true)
  }

  private def translateCheckNull(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction], ir: IR): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: null pair deferenced or freed\n")

    /* Since translation uses PrintStr, we translate PrintStr as well */
    addBranchLink(PrintStr)

    errorCheckCreation(blName, errorStr, PrintStr)
  }

  private def translateCheckDivZero(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction], ir: IR): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: division or modulo by zero\n")
      
    /* Since translation uses PrintStr, we translate PrintStr as well */
    addBranchLink(PrintStr)

    errorCheckCreation(blName, errorStr, PrintStr)
  }

  private def translateCheckOverflow(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction], ir: IR): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: integer overflow or underflow occurred\n")

    /* Since translation uses PrintStr, we translate PrintStr as well */
    addBranchLink(PrintStr)

    errorCheckCreation(blName, errorStr, PrintStr)

    addPrintFunction(errorStr,PrintStr, false)
  }

  private def translatePrintBool(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val falseStr  = StrLabel(s"${blName.getName}_str0", "false")
    val trueStr   = StrLabel(s"${blName.getName}_str1", "true")
    val formatStr = StrLabel(s"${blName.getName}_str2", "%.*s")
    
    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(falseStr)
    instrsBuffer += CreateLabel(trueStr)
    instrsBuffer += CreateLabel(formatStr)
    
    val trueLabel  = JumpLabel(s"${blName.getName}0")
    val printLabel = JumpLabel(s"${blName.getName}1")
    lableCreation("text", blName, true)
    instrsBuffer += CmpInstr(R0, Immediate(0))
    instrsBuffer += CondBranchInstr(NeqCond, trueLabel)
    
    /* Prints false */
    instrsBuffer += LoadInstr(R2, falseStr)
    instrsBuffer += BranchInstr(printLabel)
    
    /* Prints true */
    instrsBuffer += CreateLabel(trueLabel)
    instrsBuffer += LoadInstr(R2, trueStr)
    
    /* Prints string */
    instrsBuffer += CreateLabel(printLabel)
    instrsBuffer += LoadInstr(R1, RegIntOffset(R2, ArrayLenOffset))

    addPrintFunction(formatStr, PrintFormatted)
  }
  
  private def translatePrintInt(
    blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val intFormatStr = StrLabel(s"${blName.getName}_str0", "%d")
      
    /* Adds format string to data section */
    lableCreation("data", intFormatStr, false)
      
    /* Adds print_int function to text section */
    lableCreation("text", blName, true)
    instrsBuffer += MovInstr(R1, R0)

    addPrintFunction(intFormatStr, PrintFormatted)
  }

  private def translatePrintChar(
    blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val charFormatStr = StrLabel(s"${blName.getName}_str0", "%c")
    
    /* Adds format string to data section */
    lableCreation("data", charFormatStr, false)
    
    /* Adds print_char function to text section */
    lableCreation("text", blName, true)
    instrsBuffer += MovInstr(R1, R0)
    addPrintFunction(charFormatStr, PrintFormatted)
  }

  private def translatePrintLine(
    blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val lineFormatStr = StrLabel(s"${blName.getName}_str0", "")
    
    /* Adds format string to data section */
    lableCreation("data", lineFormatStr, false)
      
    /* Adds print_line function to text section */
    lableCreation("text", blName, true)

    addPrintFunction(lineFormatStr, Puts)
  }
  
  private def translatePrintPointer(
    blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val pointerFormatStr = StrLabel(s"${blName.getName}_str0", "%p")
    
    /* Adds format string to data section */
    lableCreation("data", pointerFormatStr, false)
    
    /* Adds print_pointer function to text section */
    lableCreation("text", blName, true)
    instrsBuffer += MovInstr(R1, R0)

    addPrintFunction(pointerFormatStr, PrintFormatted)
  }

  private def translatePrintStr(
    blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val strFormatStr = StrLabel(s"${blName.getName}_str0", "%.*s")
    
    /* Adds format string to data section */
    lableCreation("data", strFormatStr, false)
      
    /* Adds print_str function to text section */
    lableCreation("text", blName, true)
    instrsBuffer += MovInstr(R2, R0)
    instrsBuffer += LoadInstr(R1, RegIntOffset(R0, ArrayLenOffset))

    addPrintFunction(strFormatStr, PrintFormatted)
  }

  private def translateReadChar(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val readCharFormatStr = StrLabel(s"${blName.getName}_str0", " %c")
    val charByteOffset    = 1

    /* Adds format string to data section */
    lableCreation("data", readCharFormatStr, false)
    /* Adds read_char function to text section */
    lableCreation("text", blName, true)
    readFunction(readCharFormatStr, charByteOffset)
  }
  
  private def translateReadInt(
    blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val readIntFormatStr = StrLabel(s"${blName.getName}_str0", "%d")
    val intByteSize      = 4

    /* Adds format string to data section */
    lableCreation("data", readIntFormatStr, false)
    
    /* Adds read_int function to text section */
    lableCreation("text", blName, true)
    readFunction(readIntFormatStr, intByteSize)
  }

  private def lableCreation(
    label: String,
    format: Label,
    push: Boolean
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    if (!label.isEmpty())
      instrsBuffer += CreateLabel(SegmentLabel(label))
    instrsBuffer += CreateLabel(format)
    if (push)
      instrsBuffer += PushInstr(Seq(LR))
  }

  private def loadAndBranch(
    loadFormat: StrLabel,
    branchFormat: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    instrsBuffer += LoadInstr(R0, loadFormat)
    instrsBuffer += BranchLinkInstr(branchFormat)
  }

  private def addPrintFunction(
    loadFormat: StrLabel,
    branchFormat: FuncLabel,
    pop: Boolean = true
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    loadAndBranch(loadFormat, branchFormat)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    if (pop)
      instrsBuffer += PopInstr(Seq(PC))
  }

  private def errorCheckCreation(
    blName: FuncLabel,
    errorStr: StrLabel,
    print: FuncLabel,
    flush: Boolean = false
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    /* Adds error string to data section */
    lableCreation("data", errorStr, false)

    /* Adds error check to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    loadAndBranch(errorStr, print)
    if (flush) {
      instrsBuffer += MovInstr(R0, Immediate(0))
      instrsBuffer += BranchLinkInstr(FileFlush)
    }
    instrsBuffer += MovInstr(R0, Immediate(ErrorExitCode))
    instrsBuffer += BranchLinkInstr(ExitLabel)
  }
    
  private def readFunction (
    format: StrLabel,offset: Int
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    instrsBuffer += StoreByteInstr(R0, RegIntOffset(SP, -offset), true) // Push R0 to stack
    instrsBuffer += MovInstr(R1, SP) // Pass address of R0 to scanf
    loadAndBranch(format, ScanFormatted)
    if (offset == 1) // Pop R0 from stack
      instrsBuffer += LoadSignedByteInstr(R0, RegIntOffset(SP, 0)) 
    else if (offset == 4)
      instrsBuffer += LoadInstr(R0, RegIntOffset(SP, 0))
    instrsBuffer += AddInstr(SP, SP, Immediate(offset))
    instrsBuffer += PopInstr(Seq(PC))
  }
    
}
  