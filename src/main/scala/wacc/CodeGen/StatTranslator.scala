package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes._
import wacc.Instructions._

import ExprTranslator._
import IRBuilder._
import Utils._
import scala.collection.mutable.ListBuffer
import wacc.SemanticChecker.SymbolObjectType._
import wacc.SemanticChecker.SymbolObject._

object StatTranslator {

  def translateStatement(
      stat: Stat
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder): Unit =
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
  def translateMalloc(size: Int)(implicit ir: IRBuilder) = {
    // Allocate memory
    addInstr(MovInstr(OpRet, Immediate(size)))
    addInstr(BranchLinkInstr(MallocLabel))

    // Give the memory pointer to R12
    addInstr(MovInstr(MPtr, OpRet))
  }

  /* Translate for Conditional branch link */
  def translateCondBLink(cond: CondCode, blName: FuncLabel)(implicit ir: IRBuilder) = {
    addBranchLink(blName)
    addInstr(CondBranchLinkInstr(cond, blName))
  }

  /* Translate for Branch link */
  def translateBLink(blName: FuncLabel)(implicit ir: IRBuilder) = {
    addBranchLink(blName)
    addInstr(BranchLinkInstr(blName))
  }

  private def translateDeclare(ident: Ident, initValue: Rvalue)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IRBuilder
  ) = {
    // Calculate init value
    transRValue(initValue)

    // Pop init value to OpR1
    addInstr(PopInstr(Seq(OpR1)))

    // Move to the first available location
    val loc = stateST.nextStoreLocation()
    val size = sizeOfElem(checkExprType(ident))

    // Store value to ident
    locMovStore(size, OpR1, loc)

    // Add the location of variable to stateTable
    stateST.add(ident.name, loc)
  }

  private def declareArrayLit(
      arrayValue: ArrayLit
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    val arr_len = arrayValue.values.size
    // Array store len + 1 elems -> +1 for store length
    val arr_size = (arr_len + 1) * PtrSize

    // malloc array
    // @ (arr-len) element array
    translateMalloc(arr_size)

    // @ array pointers are shifted forwards by 4 bytes (to account for size)
    // move array pointer to a[1]
    addInstr(AddInstr(MPtr, MPtr, Immediate(PtrSize)))

    // store length of array in a[0]
    addInstr(MovInstr(OpR1, Immediate(arr_len)))
    addInstr(StoreInstr(OpR1, RegIntOffset(MPtr, -PtrSize)))

    // For loop to store each element
    for (i <- 0 until arr_len) {

      // translate Expr to Opr1
      translateExprTo(arrayValue.values(i), OpR1)

      // evaluate size of element to get size factor
      val size_factor = sizeOfElem(checkExprType(arrayValue.values(i)))

      // Store elem to a[i]
      locMovStore(size_factor, OpR1, RegIntOffset(MPtr, i * size_factor))
    }

    // Push Array pointer
    addInstr(PushInstr(Seq(MPtr)))
  }

   private def declareStructLit(
      structValue: StructLit
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    val structSizeList = ListBuffer[Int]()
    structValue.values.foreach{field =>
      val _type = checkExprType(field)
      structSizeList += sizeOfElem(_type)
    }

    // malloc struct
    translateMalloc(structSizeList.sum)

    var offsetPtr = 0
    // For loop to store each field
    for (i <- 0 until structValue.values.size) {

      // translate Expr to Opr1
      translateExprTo(structValue.values(i), OpR1)

      // Store field elem
      val fieldSize = structSizeList(i)
      locMovStore(fieldSize, OpR1, RegIntOffset(MPtr, offsetPtr))

      // Increase offset
      offsetPtr += fieldSize
    }

    // Push Array pointer
    addInstr(PushInstr(Seq(MPtr)))
  }

  private def declareNewPair(
      pairValue: NewPair
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    declareNewPairLit(pairValue.expr1)
    declareNewPairLit(pairValue.expr2)

    // Allocate for pair
    translateMalloc(PairSize)

    // Pop second elem from stack
    addInstr(PopInstr(Seq(OpR1)))
    // Store second elem
    addInstr(StoreInstr(OpR1, RegIntOffset(MPtr, PairSndIdx)))

    // Pop first elem from stack
    addInstr(PopInstr(Seq(OpR1)))
    // Store first elem
    addInstr(StoreInstr(OpR1, RegIntOffset(MPtr, PairFstIdx)))

    // Push pair pointer
    addInstr(PushInstr(Seq(MPtr)))
  }

  private def declareNewPairLit(
      elem: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    val _type = checkExprType(elem)
    val size  = sizeOfElem(_type)

    // Malloc for pair elem
    translateMalloc(size)

    // translate Expr to Opr1
    translateExprTo(elem, OpR1)
    
    locMovStore(size, OpR1, RegIntOffset(MPtr, 0))

    // Push Pair elem pointer
    addInstr(PushInstr(Seq(MPtr)))
  }

  private def storeIdent(ident: Ident)(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Pop assign value to OpR1
    addInstr(PopInstr(Seq(OpR1)))

    // Move assign value to target_loc
    val target_loc = findVarLoc(ident.name, stateST)

    val size = sizeOfElem(checkExprType(ident))
    locMovStore(size, OpR1, target_loc)
  }

  private def getPairElem(
      pairValue: PairElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder): Unit = {
    // Get pair elem pointer
    getPairElemPointer(pairValue)
    addInstr(PopInstr(Seq(OpR1)))

    val _type = checkLvalueType(pairValue)
    locMovLoad(sizeOfElem(_type), OpR1, RegIntOffset(OpR1, 0))

    // Push result
    addInstr(PushInstr(Seq(OpR1)))

  }

  private def getPairElemPointer(
      pairValue: PairElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder): Unit = {
    val innerValue = pairValue.lvalue

    var location = findLvalueLoc(pairValue, stateST)

    // pair is right associative, so extract first
    innerValue match {
      case innerValue: PairElem =>
        // Get inner pair pointer
        getPairElemPointer(innerValue)
        addInstr(PopInstr(Seq(OpR1)))

        addInstr(LoadInstr(OpR1, RegIntOffset(OpR1, 0)))
        location = OpR1
        
      case _ =>
    }

    // If pair on stack, move to R8
    val locReg = location match {
      case location: Register => location
      case _ =>
        addInstr(LoadInstr(OpR1, location))
        OpR1
    }

    // Check null for pair
    addInstr(CmpInstr(locReg, Immediate(0)))
    translateCondBLink(EqCond, CheckNull)

    // Move fst/snd pointer of pair to R8
    pairValue.index match {
      case "fst" => addInstr(LoadInstr(OpR1, RegIntOffset(locReg, PairFstIdx)))
      case "snd" => addInstr(LoadInstr(OpR1, RegIntOffset(locReg, PairSndIdx)))
    }

    // Push result
    addInstr(PushInstr(Seq(OpR1)))
  }

  private def storePairElem(
      pairValue: PairElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Load pair elem pointer to stack
    getPairElemPointer(pairValue)

    // Pop pair elem pointer to OpR2
    addInstr(PopInstr(Seq(OpR2)))

    // Pop assign value to OpR1
    addInstr(PopInstr(Seq(OpR1)))

    // Move assign value to pointer
    addInstr(StoreInstr(OpR1, RegIntOffset(OpR2, 0)))
  }

  /* Special convention for arrLoad
     R3: Array pointer
     R10: Index
     R14: General purpose */
  def loadArrayElem(
      arrayValue: ArrayElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder): Unit = {

    // find array pointer
    var array_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {

      // Pop index to R10
      translateExpr(i)
      addInstr(PopInstr(Seq(R10)))

      // Move array pointer to R3
      array_loc match {
        case array_loc: Register => addInstr(MovInstr(R3, array_loc))
        case _ => addInstr(LoadInstr(R3, array_loc))
      }

      // arrayLoad - 4 Byte; arrayLoadB - 1 Byte
      val _type = checkLvalueType(arrayValue)
      val loadBranchName =
        sizeOfElem(_type) match {
          case ByteSize => ArrayLoadB
          case DefaultSize => ArrayLoad
        }

      // branch to array load
      translateBLink(loadBranchName)

      // update array pointer
      array_loc = R3
    }

    // Move array pointer to OpR1 for push
    addInstr(MovInstr(OpR1, R3))
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
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Find array pointer
    var array_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {

      // Pop index to R10
      translateExpr(i)
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
          case ByteSize => ArrayStoreB
          case DefaultSize => ArrayStore
        }

      // branch to array load
      translateBLink(storeBranchName)

      // update array pointer
      array_loc = R3
    }
  }

  def loadStructElem(
      structValue: StructElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder): Unit = {

    // load first struct pointer
    val struct_loc = findVarLoc(structValue.ident.name, stateST)
    locMovLoad(DefaultSize, R3, struct_loc)

    // Get struct name of outer struct
    val fieldType = checkExprType(structValue.ident)
    var preStructName = fieldType match {
      case StructType(structName) => structName.name
      case _ => null
    }

    var preSymTable = st

    // For each dimension access
    for (fieldIdent <- structValue.field) {
      var offset = 0
      // Find field offset
      preSymTable.lookUpAll(preStructName, StructObjType()) match {
        case Some(obj: StructObj) =>
          val fields = obj.fields
          var index = 0
          // Calculate offset, add each previous field size
          while (fields(index)._1 != fieldIdent) {
            offset += sizeOfElem(fields(index)._2.getType())
            index += 1
          }
          preSymTable = obj.symTable
        case _ => 
      }

      // Update struct pointer
      val fieldSize = sizeOfElem(checkLvalueType(fieldIdent))
      locMovLoad(fieldSize, R3, RegIntOffset(R3, offset))

      // update previous struct name
      preStructName = fieldIdent.name
    }

    // Move array pointer to OpR1 for push
    addInstr(MovInstr(OpR1, R3))
  }

  /* Special convention for arrStore
     R3: Array pointer
     R10: Index
     R8: value
     R14: General purpose
     Return to R3 */
  // Now only find example of 1 dimension array assign
  private def storeStructElem(
      structValue: StructElem
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // load first struct pointer
    val struct_loc = findVarLoc(structValue.ident.name, stateST)
    locMovLoad(DefaultSize, R3, struct_loc)

    // Get struct name of outer struct
    val fieldType = checkExprType(structValue.ident)
    var preStructName = fieldType match {
      case StructType(structName) => structName.name
      case _ => null
    }
    var preSymTable = st

    // For each dimension access
    for (fieldIdent <- structValue.field) {

      var offset = 0
      // Find field offset
      preSymTable.lookUpAll(preStructName, StructObjType()) match {
        case Some(obj: StructObj) =>
          val fields = obj.fields
          var index = 0
          // Calculate offset, add each previous field size
          while (fields(index)._1 != fieldIdent) {
            offset += sizeOfElem(fields(index)._2.getType())
            index += 1
          }
          preSymTable = obj.symTable
        case _ => 
      }

      // Pop assign value into R8
      addInstr(PopInstr(Seq(R8)))

      // Update struct pointer
      val fieldSize = sizeOfElem(checkLvalueType(fieldIdent))
      locMovStore(fieldSize, R8, RegIntOffset(R3, offset))

      // update previous struct name
      preStructName = fieldIdent.name
    }

    // Move array pointer to OpR1 for push
    addInstr(MovInstr(OpR1, R3))
  }

  private def translateCall(
      callValue: Call
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // If this is inside a function with parameter, push caller saved regs first
    val usedParam = stateST.getUsedParamRegs()
    if (!usedParam.isEmpty) {
      addInstr(PushInstr(usedParam))

      // Make R12 the secondary SP
      addInstr(MovInstr(SFP, SP))
      // Update stateTable for parameters -> now on stack
      stateST.updateParamToStack()
    }

    // Store parameter
    val para_num = callValue.args.size
    var index    = 0
    while (index < para_num) {
      // Store param in OpR1
      translateExprTo(callValue.args(index), OpR1)

      // parameters in regs
      if (index < paramReg.size) {

        // Change it later, should have pool of usable register
        val reg =
          index match {
            case 0 => Param1
            case 1 => Param2
            case 2 => Param3
          }
        addInstr(MovInstr(reg, OpR1))

      } else {
        // More parameters
        // Push OpR1
        addInstr(StoreInstr(OpR1, RegIntOffset(SP, -PtrSize), writeBack = true))
      }

      index += 1
    }

    // Create branch jump
    addInstr(BranchLinkInstr(WACCFuncLabel(callValue.ident.name)))

    // Store the result in OpR1
    addInstr(MovInstr(OpR1, OpRet))

    // Add stackSpace back for parameter
    val stackSpace = (para_num - paramReg.size) * PtrSize
    if (stackSpace > 0) {
      addInstr(AddInstr(SP, SP, Immediate(stackSpace)))
    }

    if (!usedParam.isEmpty) {
      addInstr(PopInstr(usedParam))
      stateST.updateParamBackToReg()
    }

    addInstr(PushInstr(Seq(OpR1)))
  }

  private def translateAssign(target: Lvalue, newValue: Rvalue)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IRBuilder
  ) = {
    transRValue(newValue)

    // New value now on stack

    target match {
      case target: Ident     => storeIdent(target)
      case target: ArrayElem => storeArrayElem(target)
      case target: PairElem  => storePairElem(target)
      case target: StructElem => storeStructElem(target)
    }
  }

  /* Exit will return value in R0 */
  private def translateExit(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Pop result to R0 for exit
    translateExprTo(expr, OpRet)

    translateBLink(ExitLabel)
  }

  /* Return will return value in R0 */
  private def translateReturn(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Pop result to R0 for return
    translateExprTo(expr, OpRet)

    endBlock(restoreSP = true)
  }


  /* Print will print value in R0 */
  private def translatePrint(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Translate expression and store result in OpR1
    translateExprTo(expr, OpR1)

    // Caller save parameter registers
    callerSavePush()

    // Move value from OpR1 to R0 for printing
    addInstr(MovInstr(OpRet, OpR1))

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
  }

  /* Println will print value in R0 */
  private def translatePrintln(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    translatePrint(expr)
    callerSavePush()
    translateBLink(PrintLine)
    callerSavePop()
  }

  /* Read will read value to R0 */
  private def translateRead(
      lvalue: Lvalue
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    val _type = checkLvalueType(lvalue)
    val readType = _type match {
      case IntType()  => ReadInt
      case CharType() => ReadChar
      case _          => null
    }

    callerSavePush()

    lvalue match {
      case lvalue: Ident =>
        // Pop pointer to R0
        translateExprTo(lvalue, OpRet)

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(OpRet)))

        // Store read value to ident
        storeIdent(lvalue)

      case lvalue: ArrayElem =>
        // Pop pointer to R0
        translateExprTo(lvalue, OpRet)

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(OpRet)))

        // Store read value in ArrayElem
        storeArrayElem(lvalue)

      case lvalue: PairElem =>
        getPairElemPointer(lvalue)
        // Pop pair elem pointer to R0
        addInstr(PopInstr(Seq(OpRet)))

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(OpRet)))

        // Store read value in PairElem
        storePairElem(lvalue)
      
      case lvalue: StructElem =>
        translateExprTo(lvalue, OpRet)
        // Pop struct elem pointer to R0
        addInstr(PopInstr(Seq(OpRet)))

        // Read from input
        translateBLink(readType)

        // Push R0 as assign value
        addInstr(PushInstr(Seq(OpRet)))

        // Store read value in StructElem
        storeStructElem(lvalue)
    }
    callerSavePop()
  }

  /* Free will free value in R0 */
  /* Free can be appied to array and pair */
  private def translateFree(expr: Expr)(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    callerSavePush()

    /* Pop pointer to r0 */
    translateExprTo(expr, OpRet)

    // Check free pair or array
    val _type = checkExprType(expr)
    val freeType = _type match {
      case PairType(_, _) => FreePair
      case ArrayType(_)   => {
        // array pointers are shifted forward by 4 bytes
        // correct it back to original pointer before free
        addInstr(SubInstr(OpRet, OpRet, Immediate(PtrSize)))
        FreeLabel
      }
      case StructType(_)  => FreeLabel
      case _              => null
    }

    /* Jump to free */
    translateBLink(freeType)
    callerSavePop()
  }

  /* For Bool or Ident condition, add compare instr */
  private def translateBoolCond(expr: Expr)(implicit ir: IRBuilder) = {
    // Pop cond extr to OpR1
    addInstr(PopInstr(Seq(OpR1)))

    expr match {
      case Ident(_)   => addInstr(CmpInstr(OpR1, TrueImm))
      case BoolLit(_) => addInstr(CmpInstr(OpR1, TrueImm))
      case _          =>
    }
    
  }

  private def translateCondition(expr: Expr)(implicit st: SymbolTable, 
                                                      stateST: StateTable, 
                                                      ir: IRBuilder) = {
    // Translate condition expr
    translateExpr(expr)
    // Translate extra compare instruction if boolean condition
    translateBoolCond(expr)
  }

  /* New scope will have new state table */
  private def translateIf(expr: Expr, stats1: List[Stat], stats2: List[Stat])(implicit
      st: SymbolTable, 
      stateST: StateTable,
      ir: IRBuilder
  ) = {
    // Allocate new branch name
    val branch_0 = getJumpLabel()
    val branch_1 = getJumpLabel()

    // Translate condition
    translateCondition(expr)

    // if true, branch to stat1 (if true branch)
    addInstr(CondBranchInstr(checkCondCode(expr), branch_0))

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
      ir: IRBuilder
  ) = {
    // Allocate new branch name
    val branch_0 = getJumpLabel()

    val branch_1 = getJumpLabel()

    // First, unconditionally jump to Branch 1
    addInstr(BranchInstr(branch_0))

    // Translate Branch 2 here
    // .L1:
    addInstr(CreateLabel(branch_1))
    val new_stateST = new StateTable(Some(stateST))
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir))

    // .L0:
    addInstr(CreateLabel(branch_0))

    // Translate condition
    translateCondition(expr)

    // If condition is true, jump back to branch 2
    addInstr(CondBranchInstr(checkCondCode(expr), branch_1))

    // Rest of the code goes here
  }

  /* Match Compare Instruction to CondCode */
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
  private def translateBegin(stats: List[Stat])(implicit stateST: StateTable, ir: IRBuilder) = {
    /* Create new state table */
    val new_stateST = new StateTable(Some(stateST))
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir))
  }
  
  /* Translate RValue and push result to Stack */
  private def transRValue(value: Rvalue)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IRBuilder
  ) = {
    value match {
      case initValue: Expr     => translateExpr(initValue)
      case initValue: ArrayLit => declareArrayLit(initValue)
      case initValue: NewPair  => declareNewPair(initValue)
      case initValue: PairElem => getPairElem(initValue)
      case initValue: Call     => translateCall(initValue)
      case initValue: StructLit => declareStructLit(initValue)
      
    }

    // Will push to stack in every case
  }
}
