package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes._
import wacc.Instructions._

import ExprTranslator._
import IR._
import Utils._

object StatTranslator {

  def translateStatement(
      stat: Stat
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR): Unit =
    stat match {
      case Declare(type1, ident, initValue) => translateDeclare(ident, initValue)
      case Assign(target, newValue)         => translateAssign(target, newValue)
      case Read(lvalue)                     => translateRead(lvalue)
      case Free(expr)                       => translateFree(expr)
      case Return(expr)                     => translateReturn(expr)
      case Exit(expr)                       => translateExit(expr)
      case Print(expr)                      => translatePrint(expr)
      case Println(expr)                    => translatePrintln(expr)
      case If(expr, stat1, stat2)           => translateIf(expr, stat1, stat2)
      case While(expr, stat)                => translateWhile(expr, stat)
      case Begin(stat)                      => translateBegin(stat)
      case _                                =>
    }

  /* Translate for malloc part */
  def translateMalloc(size: Int)(implicit ir: IR) = {
    // Allocate memory
    addInstr(MovInstr(R0, Immediate(size)))
    addInstr(BranchLinkInstr(MallocLabel))

    // Give the memory pointer to R12
    addInstr(MovInstr(R12, R0))
  }

  /* Translate for Conditional branch link */
  def translateCondBLink(cond: CondCode, blName: FuncLabel)(implicit ir: IR) = {

    addBranchLink(blName)
    addInstr(CondBranchLinkInstr(cond, blName))
  }

  /* Translate for Branch link */
  def translateBLink(blName: FuncLabel)(implicit ir: IR) = {
    addBranchLink(blName)

    addInstr(BranchLinkInstr(blName))
  }

  private def translateDeclare(ident: Ident, initValue: Rvalue)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IR
  ) = {
    initValue match {
      case initValue: Expr     => translateExpr(initValue)
      case initValue: ArrayLit => declareArrayLit(initValue)
      case initValue: NewPair  => declareNewPair(initValue)
      case initValue: PairElem => getPairElem(initValue)
      case initValue: Call     => translateCall(initValue)
    }

    // Pop result
    addInstr(PopInstr(Seq(R8)))

    // Move to the first available location
    val loc = stateST.nextStoreLocation()
    loc match {
      case loc: Register => addInstr(MovInstr(loc, R8))
      case _             => {
        val size = sizeOfElem(checkExprType(ident))
        size match {
          case 1 => addInstr(StoreByteInstr(R8, loc))
          case 4 => addInstr(StoreInstr(R8, loc))
        }
      }
    }

    // Add the location of variable to stateTable
    stateST.add(ident.name, loc)
    addInstr(Comment(s"Declared variable: current variables num ${stateST.getUsedRegs().size}"))
  }

  private def declareArrayLit(
      arrayValue: ArrayLit
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    val arr_len = arrayValue.values.size
    // Array store len + 1 elems -> +1 for store length
    val arr_size = (arr_len + 1) * 4

    // malloc array
    // @ (arr-len) element array
    translateMalloc(arr_size)

    // @ array pointers are shifted forwards by 4 bytes (to account for size)
    // move array pointer to a[1]
    addInstr(AddInstr(R12, R12, Immediate(4)))

    // store length of array in a[0]
    addInstr(MovInstr(R8, Immediate(arr_len)))
    addInstr(StoreInstr(R8, RegIntOffset(R12, -4)))

    // For loop to store each element
    for (i <- 0 until arr_len) {
      // Load elem into R8
      translateExpr(arrayValue.values(i))

      // Pop result
      addInstr(PopInstr(Seq(R8)))

      // evaluate size of element to get size factor
      val size_factor = sizeOfElem(checkExprType(arrayValue.values(i)))

      // Store elem to a[i]
      size_factor match {
        case 1 => addInstr(StoreByteInstr(R8, RegIntOffset(R12, i * size_factor)))
        case 4 => addInstr(StoreInstr(R8, RegIntOffset(R12, i * size_factor)))
      }

    }

    // Push Array pointer
    addInstr(PushInstr(Seq(R12)))
  }

  private def declareNewPair(
      pairValue: NewPair
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    declareNewPairLit(pairValue.expr1)
    declareNewPairLit(pairValue.expr2)

    // Allocate for pair
    translateMalloc(8)

    // Pop second elem from stack
    addInstr(PopInstr(Seq(R8)))
    // Store second elem
    addInstr(StoreInstr(R8, RegIntOffset(R12, 4)))

    // Pop first elem from stack
    addInstr(PopInstr(Seq(R8)))
    // Store first elem
    addInstr(StoreInstr(R8, RegIntOffset(R12, 0)))

    // Push pair pointer
    addInstr(PushInstr(Seq(R12)))
  }

  private def declareNewPairLit(
      elem: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {

    val _type = checkExprType(elem)
    val size  = sizeOfElem(_type)

    // Malloc for pair elem
    translateMalloc(size)

    // Load value of elem
    translateExpr(elem)
    // Pop result
    addInstr(PopInstr(Seq(R8)))

    size match {
      case 1 => addInstr(StoreByteInstr(R8, RegIntOffset(R12, 0)))
      case 4 => addInstr(StoreInstr(R8, RegIntOffset(R12, 0)))
    }

    // Push Pair elem pointer
    addInstr(PushInstr(Seq(R12)))
  }

  private def storeIdent(ident: Ident)(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    // assign value now on stack

    // Pop assign value to R8
    addInstr(PopInstr(Seq(R8)))

    // Move assign value to target_loc
    val target_loc = findVarLoc(ident.name, stateST)

    target_loc match {
      case target_loc: Register => addInstr(MovInstr(target_loc, R8))
      case _             => {
        val size = sizeOfElem(checkExprType(ident))
        size match {
          case 1 => addInstr(StoreByteInstr(R8, target_loc))
          case 4 => addInstr(StoreInstr(R8, target_loc))
        }
      }
    }
  }

  private def getPairElem(
      pairValue: PairElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR): Unit = {
    // Get pair elem pointer
    getPairElemPointer(pairValue)
    addInstr(PopInstr(Seq(R8)))

    val _type = checkLvalueType(pairValue)
    sizeOfElem(_type) match {
      case 1 => addInstr(LoadSignedByteInstr(R8, RegIntOffset(R8, 0)))
      case 4 => addInstr(LoadInstr(R8, RegIntOffset(R8, 0)))
    }

    // // Load actual pair elem
    // addInstr(LoadInstr(R8, RegIntOffset(R8, 0)))

    // Push result
    addInstr(PushInstr(Seq(R8)))

  }

  private def getPairElemPointer(
      pairValue: PairElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR): Unit = {
    val innerValue = pairValue.lvalue

    var location = findLvalueLoc(pairValue, stateST)

    // pair is right associative, so extract first
    innerValue match {
      case innerValue: PairElem =>
        getPairElemPointer(innerValue)

        // TODO: Why are these added to get extract correct?
        addInstr(PopInstr(Seq(R8)))
        addInstr(LoadInstr(R8, RegIntOffset(R8, 0)))
        location = R8
        
      case _ =>
    }

    // If pair on stack, move to R8
    val locReg = location match {
      case location: Register => location
      case _ =>
        addInstr(LoadInstr(R8, location))
        R8
    }

    // Check null for pair
    addInstr(CmpInstr(locReg, Immediate(0)))
    translateCondBLink(EqCond, CheckNull)

    // Move fst/snd pointer of pair to R8
    pairValue.index match {
      case "fst" => addInstr(LoadInstr(R8, RegIntOffset(locReg, 0)))
      case "snd" => addInstr(LoadInstr(R8, RegIntOffset(locReg, 4)))
    }

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

  private def storePairElem(
      pairValue: PairElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    // Assign value on stack now

    // Load pair elem pointer to stack
    getPairElemPointer(pairValue)

    // Pop pair elem pointer to R9
    addInstr(PopInstr(Seq(R9)))

    // Pop assign value to R8
    addInstr(PopInstr(Seq(R8)))

    // Move assign value to pointer
    addInstr(StoreInstr(R8, RegIntOffset(R9, 0)))
  }

  /* Special convention for arrLoad
     R3: Array pointer
     R10: Index
     R14: General purpose */
  def loadArrayElem(
      arrayValue: ArrayElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR): Unit = {

    // find array pointer
    var array_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {

      // Calculate index and push to stack
      i match {
        case i: ArrayElem =>
          // previous array pointer is this index
          loadArrayElem(i)
        case _ =>
          // calculate index from expr
          translateExpr(i)
      }

      // Move array pointer to R3
      array_loc match {
        case array_loc: Register => addInstr(MovInstr(R3, array_loc))
        case _ => addInstr(LoadInstr(R3, array_loc))
      }

      // Pop index to R10
      addInstr(PopInstr(Seq(R10)))

      // arrayLoad - 4 Byte; arrayLoadB - 1 Byte
      val _type = checkLvalueType(arrayValue)
      val loadBranchName =
        sizeOfElem(_type) match {
          case 1 => ArrayLoadB
          case 4 => ArrayLoad
        }

      // branch to array load
      translateBLink(loadBranchName)

      // update array pointer
      array_loc = R3
    }

    // Push result
    addInstr(PushInstr(Seq(R3)))
  }

  /* Special convention for arrStore
     R3: Array pointer
     R10: Index
     R8: value
     R14: General purpose
     Return to R3 */
  // Now only find example of 1 dimension array assign
  private def storeArrayElem(
      arrayValue: ArrayElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {

    // New value on stack

    // Find array pointer
    var array_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {

      // Push index
      translateExpr(i)
      // Pop index into R10
      addInstr(PopInstr(Seq(R10)))

      // Pop assign value into R8
      addInstr(PopInstr(Seq(R8)))

      // Move array pointer to R3
      array_loc match {
        case array_loc: Register => addInstr(MovInstr(R3, array_loc))
        case _ => addInstr(LoadInstr(R3, array_loc))
      }

      // arrayStore - 4 Byte; arrayStoreB - 1 Byte
      val _type = checkLvalueType(arrayValue)
      val storeBranchName =
        sizeOfElem(_type) match {
          case 1 => ArrayStoreB
          case 4 => ArrayStore
        }

      // branch to array load
      translateBLink(storeBranchName)

      // update array pointer
      array_loc = R3
    }
  }

  private def translateCall(
      callValue: Call
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    // Store parameter
    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val para_len = callValue.args.size
    var index    = 0

    addInstr(Comment("start of translate call"))
    // If this is inside a function with parameter, push caller saved regs first
    val usedParam = stateST.getUsedParamRegs()
    if (!usedParam.isEmpty) {
      addInstr(PushInstr(usedParam))

      // Make R12 the secondary SP
      addInstr(MovInstr(SFP, SP))
      // Update stateTable for parameters -> now on stack
      stateST.updateParamToStack()
    }

    while (index < para_len) {

      // Pop parameter to R8
      translateExpr(callValue.args(index))
      addInstr(PopInstr(Seq(R8)))

      // First 3 parameters
      if (index < paramReg.size) {

        // Change it later, should have pool of usable register
        val reg =
          index match {
            case 0 => R0
            case 1 => R1
            case 2 => R2
          }
        addInstr(MovInstr(reg, R8))
      } else {
        // More parameters
        // Push R8
        addInstr(StoreInstr(R8, RegIntOffset(SP, -4), writeBack = true))
      }

      index += 1
    }

    // Create branch jump
    addInstr(BranchLinkInstr(WACCFuncLabel(callValue.ident.name)))

    // Store the result in R8
    addInstr(MovInstr(R8, R0))

    // Add stackSpace back for parameter
    val stackSpace = (para_len - paramReg.size) * 4
    if (stackSpace > 0) {
      addInstr(AddInstr(SP, SP, Immediate(stackSpace)))
    }

    // Push function return value
    // addInstr(PushInstr(Seq(R0)))

    if (!usedParam.isEmpty) {
      addInstr(PopInstr(usedParam))
      stateST.updateParamBackToReg()
    }

    addInstr(PushInstr(Seq(R8)))
    addInstr(Comment("end of translate call"))
  }

  private def translateAssign(target: Lvalue, newValue: Rvalue)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IR
  ) = {
    newValue match {
      case initValue: Expr     => translateExpr(initValue)
      case initValue: ArrayLit => declareArrayLit(initValue)
      case initValue: NewPair  => declareNewPair(initValue)
      case initValue: PairElem => getPairElem(initValue)
      case initValue: Call     => translateCall(initValue)
    }

    // New value now on stack

    target match {
      case target: Ident     => storeIdent(target)
      case target: ArrayElem => storeArrayElem(target)
      case target: PairElem  => storePairElem(target)
    }

  }

  /* Exit will return value in R0 */
  private def translateExit(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    translateExpr(expr)

    // Pop result to R0 for exit
    addInstr(PopInstr(Seq(R0)))

    translateBLink(ExitLabel)
  }

  /* Return will return value in R0 */
  private def translateReturn(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    translateExpr(expr)

    // Pop result to R0 for return
    addInstr(PopInstr(Seq(R0)))

    endBlock(restoreSP = true)
  }


  /* Print will print value in R0 */
  private def translatePrint(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    // Translate expression and store result in R8
    translateExpr(expr)
    addInstr(PopInstr(Seq(R8)))

    addInstr(Comment("In translate print, after translating expression"))

    // Caller save parameter registers
    callerSavePush()

    // Move value from R8 to R0 for printing
    addInstr(MovInstr(R0, R8))

    val _type = checkExprType(expr)
    val printType = _type match {
      case IntType()             => PrintInt
      case BoolType()            => PrintBool
      case CharType()            => PrintChar
      case StrType()             => PrintStr
      case ArrayType(CharType()) => PrintStr
      case _                     => PrintPointer
    }

    translateBLink(printType)
    callerSavePop()

    addInstr(Comment("Done translating print"))
  }

  /* Println will print value in R0 */
  private def translatePrintln(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {
    translatePrint(expr)
    callerSavePush()
    translateBLink(PrintLine)
    callerSavePop()
  }

  /* Read will read value to R0 */
  private def translateRead(
      lvalue: Lvalue
  )(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {

    val _type = checkLvalueType(lvalue)
    val readType = _type match {
      case IntType()  => ReadInt
      case CharType() => ReadChar
      case _          => null
    }

    callerSavePush()

    lvalue match {
      case lvalue: Ident =>
        translateExpr(lvalue)
        // Pop original value to R0
        addInstr(PopInstr(Seq(R0)))

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(R0)))

        storeIdent(lvalue)
      case lvalue: ArrayElem =>
        translateExpr(lvalue)
        // Pop original value to R0
        addInstr(PopInstr(Seq(R0)))

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(R0)))

        // Store read value in ArrayElem
        storeArrayElem(lvalue)
      case lvalue: PairElem =>
        getPairElemPointer(lvalue)
        // Pop pair elem pointer to R0
        addInstr(PopInstr(Seq(R0)))

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(R0)))

        // Store read value in PairElem
        storePairElem(lvalue)

    }
    callerSavePop()
  }

  /* Free will free value in R0 */
  /* Free can be appied to array and pair */
  private def translateFree(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IR) = {

    callerSavePush()

    /* Pop pointer to r0 */
    translateExpr(expr)
    addInstr(PopInstr(Seq(R0)))

    // Check free pair or array
    val _type = checkExprType(expr)
    val freeType = _type match {
      case PairType(_, _) => FreePair
      case ArrayType(_)   => {
        // array pointers are shifted forward by 4 bytes
        // correct it back to original pointer before free
        addInstr(SubInstr(R0, R0, Immediate(4)))
        FreeLabel
      }
      case _              => null
    }

    /* Jump to free */
    translateBLink(freeType)
    callerSavePop()
  }

  private def translateBoolCond(expr: Expr)(implicit ir: IR) = {
    addInstr(PopInstr(Seq(R8)))
    expr match {
      case Ident(_)   => addInstr(CmpInstr(R8, Immediate(1)))
      case BoolLit(_) => addInstr(CmpInstr(R8, Immediate(1)))
      case _          =>
    }
    
  }

  /* New scope will have new state table */
  private def translateIf(expr: Expr, stats1: List[Stat], stats2: List[Stat])(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IR
  ) = {
    // Translate condition
    translateExpr(expr)

    translateBoolCond(expr)

    // Allocate new branch name
    val branch_0 = JumpLabel(s"${getBranchCounter()}")
    incBranchCounter()

    val branch_1 = JumpLabel(s"${getBranchCounter()}")
    incBranchCounter()

    // if true, branch to stat1 (if true branch)
    addInstr(CondBranchInstr(checkCondCode(expr), branch_0))

    // Somewhere create Branch1 later --> translate stat1 there

    // if false, continue executing stat2 (else branch)
    /* Create new state table */
    val new_stateST2 = new StateTable(Some(stateST))
    stats2.foreach(s => translateStatement(s)(s.symb, new_stateST2, ir))

    // As Branch1 will be below, when false, skip to Branch 2 (will be the rest of code)
    addInstr(BranchInstr(branch_1))

    // Translate branch1
    // .L0:
    addInstr(CreateLabel(branch_0))

    // Execute stat1
    val new_stateST1 = new StateTable(Some(stateST))
    stats1.foreach(s => translateStatement(s)(s.symb, new_stateST1, ir))

    // The rest of code needs to be in branch2
    // .L1:
    addInstr(CreateLabel(branch_1))
  }

  /* New scope will have new state table */
  private def translateWhile(expr: Expr, stats: List[Stat])(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IR
  ) = {
    addInstr(Comment("Start of translate while"))
    // Allocate new branch name
    val branch_0 = JumpLabel(s"${getBranchCounter()}")
    incBranchCounter()

    val branch_1 = JumpLabel(s"${getBranchCounter()}")
    incBranchCounter()

    // First, unconditionally jump to Branch 1
    addInstr(BranchInstr(branch_0))

    // Translate Branch 2 here
    // .L1:
    addInstr(CreateLabel(branch_1))
    val new_stateST = new StateTable(Some(stateST))
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir))

    // .L0:
    // Translate condition
    addInstr(CreateLabel(branch_0))
    translateExpr(expr)

    translateBoolCond(expr)

    // may need to specially check when expr: bool

    // If condition is true, jump back to branch 2
    addInstr(CondBranchInstr(checkCondCode(expr), branch_1))

    // Rest of the code goes here
  }

  private def checkCondCode(cond: Expr): CondCode =
    cond match {
      case Eq(_, _)  => EqCond
      case Neq(_, _) => NeqCond
      case Gt(_, _)  => GtCond
      case Gte(_, _) => GteCond
      case Lt(_, _)  => LtCond
      case Lte(_, _) => LteCond
      case _         => EqCond
    }

  /* New scope will have new state table */
  private def translateBegin(stats: List[Stat])(implicit stateST: StateTable, ir: IR) = {
    /* Create new state table */
    val new_stateST = new StateTable(Some(stateST))
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir))
  }
}
