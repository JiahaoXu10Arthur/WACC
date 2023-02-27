package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._

object BranchLinkTranslator {
	final val ErrorExitCode = 255

  def translateBranchLink(blName: FuncLabel): (List[Instruction], List[FuncLabel]) = {
    implicit val instrsBuffer = mutable.ListBuffer[Instruction]()
    implicit val blNameBuffer = mutable.ListBuffer[FuncLabel]()

    blName match {
      case ArrayLoad     => translateArrayLoad(blName)
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

    (instrsBuffer.toList, blNameBuffer.toList)
  }

  /*
	Special calling convention: array ptr passed in R3, index in R10,
	LR (R14) is used as general register, and return into R3
   */
  private def translateArrayLoad(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction],
             bLNameBuffer: mutable.ListBuffer[FuncLabel]): Unit = {
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += CmpInstr(R10, Immediate(0))
    instrsBuffer += CondMovInstr(LtCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(LtCond, CheckBound)
    instrsBuffer += LoadInstr(LR, RegIntOffset(R3, -4)) // TODO: Why -4?
    instrsBuffer += CmpInstr(R10, LR)
    instrsBuffer += CondMovInstr(GteCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(GteCond, CheckBound)
    instrsBuffer += LoadInstr(R3, RegShiftOffset(R3, R10, LSL(2)))
    instrsBuffer += PopInstr(Seq(PC))

    bLNameBuffer += CheckBound
  }

  /*
	Special calling convention: array ptr passed in R3, index in R10,
	value to store in R8, LR (R14) is used as general register
   */
  private def translateArrayStore(
      blName: FuncLabel,
      storeByte: Boolean = false
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction],
             bLNameBuffer: mutable.ListBuffer[FuncLabel]): Unit = {
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += CmpInstr(R10, Immediate(0))
    instrsBuffer += CondMovInstr(LtCond, R1, R10)
    instrsBuffer += CondBranchLinkInstr(LtCond, CheckBound)
    instrsBuffer += LoadInstr(LR, RegIntOffset(R3, -4)) // TODO: Why -4?
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

    bLNameBuffer += CheckBound
  }

  private def translateFreePair(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction],
             bLNameBuffer: mutable.ListBuffer[FuncLabel]): Unit = {
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
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

    bLNameBuffer += CheckNull
  }

  private def translateCheckBound(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: array index %d out of bounds\n")

		/* Adds error string to data section */
		instrsBuffer += CreateLabel(SegmentLabel("data"))
		instrsBuffer += CreateLabel(errorStr)

		/* Adds error check to text section */
		instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
		instrsBuffer += LoadInstr(R0, errorStr)
		instrsBuffer += BranchLinkInstr(PrintFormatted)
		instrsBuffer += MovInstr(R0, Immediate(0))
		instrsBuffer += BranchLinkInstr(FileFlush)
		instrsBuffer += MovInstr(R0, Immediate(ErrorExitCode))
    instrsBuffer += BranchLinkInstr(ExitLabel)
  }

	private def translateCheckNull(
		blName: FuncLabel
	)(implicit instrsBuffer: mutable.ListBuffer[Instruction],
             bLNameBuffer: mutable.ListBuffer[FuncLabel]): Unit = {
	  val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: null pair deferenced or freed\n")

		/* Adds error string to data section */
		instrsBuffer += CreateLabel(SegmentLabel("data"))
		instrsBuffer += CreateLabel(errorStr)

		/* Adds error check to text section */
		instrsBuffer += CreateLabel(SegmentLabel("text"))
		instrsBuffer += CreateLabel(blName)
		instrsBuffer += LoadInstr(R0, errorStr)
		instrsBuffer += BranchLinkInstr(PrintStr)
		instrsBuffer += MovInstr(R0, Immediate(ErrorExitCode))
		instrsBuffer += BranchLinkInstr(ExitLabel)

    bLNameBuffer += PrintStr
	}

	private def translateCheckDivZero(
		blName: FuncLabel
	)(implicit instrsBuffer: mutable.ListBuffer[Instruction],
             bLNameBuffer: mutable.ListBuffer[FuncLabel]): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: division or modulo by zero\n")

		/* Adds error string to data section */
		instrsBuffer += CreateLabel(SegmentLabel("data"))
		instrsBuffer += CreateLabel(errorStr)

		/* Adds error check to text section */
		instrsBuffer += CreateLabel(SegmentLabel("text"))
		instrsBuffer += CreateLabel(blName)
		instrsBuffer += LoadInstr(R0, errorStr)
		instrsBuffer += BranchLinkInstr(PrintStr)
		instrsBuffer += MovInstr(R0, Immediate(ErrorExitCode))
		instrsBuffer += BranchLinkInstr(ExitLabel)

    bLNameBuffer += PrintStr
	}

	private def translateCheckOverflow(
		blName: FuncLabel
	)(implicit instrsBuffer: mutable.ListBuffer[Instruction],
             bLNameBuffer: mutable.ListBuffer[FuncLabel]): Unit = {
    val errorStr =
      StrLabel(s"${blName.getName}_str0", "fatal error: integer overflow or underflow occurred\n")

		/* Adds error string to data section */
		instrsBuffer += CreateLabel(SegmentLabel("data"))
		instrsBuffer += CreateLabel(errorStr)

		/* Adds error check to text section */
		instrsBuffer += CreateLabel(SegmentLabel("text"))
		instrsBuffer += CreateLabel(blName)
		instrsBuffer += LoadInstr(R0, errorStr)
		instrsBuffer += BranchLinkInstr(PrintStr)
		instrsBuffer += MovInstr(R0, Immediate(ErrorExitCode))
		instrsBuffer += BranchLinkInstr(ExitLabel)

    bLNameBuffer += PrintStr
	}

  private def translatePrintBool(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val falseStr = StrLabel(s"${blName.getName}_str0", "false")
    val trueStr = StrLabel(s"${blName.getName}_str1", "true")
    val formatStr = StrLabel(s"${blName.getName}_str2", "%.*s")

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(falseStr)
    instrsBuffer += CreateLabel(trueStr)
    instrsBuffer += CreateLabel(formatStr)

    val trueLabel = JumpLabel(s"${blName.getName}0")
    val printLabel = JumpLabel(s"${blName.getName}1")
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
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
    instrsBuffer += LoadInstr(R1, RegIntOffset(R2, -4)) // TODO: why -4?
    instrsBuffer += LoadInstr(R0, formatStr)
    instrsBuffer += BranchLinkInstr(PrintFormatted)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translatePrintInt(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val intFormatStr = StrLabel(s"${blName.getName}_str0", "%d")

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(intFormatStr)

    /* Adds print_int function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += MovInstr(R1, R0)
    instrsBuffer += LoadInstr(R0, intFormatStr)
    instrsBuffer += BranchLinkInstr(PrintFormatted)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translatePrintChar(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val charFormatStr = StrLabel(s"${blName.getName}_str0", "%c")

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(charFormatStr)

    /* Adds print_char function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += MovInstr(R1, R0)
    instrsBuffer += LoadInstr(R0, charFormatStr)
    instrsBuffer += BranchLinkInstr(PrintFormatted)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translatePrintLine(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val lineFormatStr = StrLabel(s"${blName.getName}_str0", "")

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(lineFormatStr)

    /* Adds print_line function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += LoadInstr(R0, lineFormatStr)
    instrsBuffer += BranchLinkInstr(Puts)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translatePrintPointer(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val pointerFormatStr = StrLabel(s"${blName.getName}_str0", "%p")

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(pointerFormatStr)

    /* Adds print_pointer function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += MovInstr(R1, R0)
    instrsBuffer += LoadInstr(R0, pointerFormatStr)
    instrsBuffer += BranchLinkInstr(PrintFormatted)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translatePrintStr(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val strFormatStr = StrLabel(s"${blName.getName}_str0", "%.*s")

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(strFormatStr)

    /* Adds print_str function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += MovInstr(R2, R0)
    instrsBuffer += LoadInstr(R1, RegIntOffset(R0, -4)) // TODO: why -4?
    instrsBuffer += LoadInstr(R0, strFormatStr)
    instrsBuffer += BranchLinkInstr(PrintFormatted)
    instrsBuffer += MovInstr(R0, Immediate(0))
    instrsBuffer += BranchLinkInstr(FileFlush)
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translateReadChar(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val readCharFormatStr = StrLabel(s"${blName.getName}_str0", "%c")
    val charByteOffset = 1

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(readCharFormatStr)

    /* Adds read_char function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += StoreByteInstr(R0, RegIntOffset(SP, -charByteOffset), true) // Push R0 to stack
    instrsBuffer += MovInstr(R1, SP) // Pass address of R0 to scanf
    instrsBuffer += LoadInstr(R0, readCharFormatStr)
    instrsBuffer += BranchLinkInstr(ScanFormatted)
    instrsBuffer += LoadSignedByteInstr(R0, RegIntOffset(SP, 0)) // Pop R0 from stack
    instrsBuffer += AddInstr(SP, SP, Immediate(charByteOffset))
    instrsBuffer += PopInstr(Seq(PC))
  }

  private def translateReadInt(
      blName: FuncLabel
  )(implicit instrsBuffer: mutable.ListBuffer[Instruction]): Unit = {
    val readIntFormatStr = StrLabel(s"${blName.getName}_str0", "%d")
    val intByteSize = 4

    /* Adds format string to data section */
    instrsBuffer += CreateLabel(SegmentLabel("data"))
    instrsBuffer += CreateLabel(readIntFormatStr)

    /* Adds read_int function to text section */
    instrsBuffer += CreateLabel(SegmentLabel("text"))
    instrsBuffer += CreateLabel(blName)
    instrsBuffer += PushInstr(Seq(LR))
    instrsBuffer += StoreInstr(R0, RegIntOffset(SP, -intByteSize), true) // Push R0 to stack
    instrsBuffer += MovInstr(R1, SP) // Pass address of R0 to scanf
    instrsBuffer += LoadInstr(R0, readIntFormatStr)
    instrsBuffer += BranchLinkInstr(ScanFormatted)
    instrsBuffer += LoadInstr(R0, RegIntOffset(SP, 0)) // Pop R0 from stack
    instrsBuffer += AddInstr(SP, SP, Immediate(intByteSize))
    instrsBuffer += PopInstr(Seq(PC))
  }

}
