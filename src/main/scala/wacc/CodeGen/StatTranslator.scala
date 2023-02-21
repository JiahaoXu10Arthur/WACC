package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.Instructions._
import wacc.SemanticChecker.SemanticTypes._

import ExprTranslator._

import scala.collection.mutable.ListBuffer

object StatTranslator {

  var branchCounter = 0

  def translateStatement(
      stat: Stat
    )(implicit st: SymbolTable,
               stateST: StateTable,
               ins: ListBuffer[Instruction]): Unit = {
    stat match {
      case Skip()                           =>
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
    }

  }

  /* Translate for malloc part */
  def translateMalloc(size: Int)(implicit st: SymbolTable,
                                          stateST: StateTable,
                                          ins: ListBuffer[Instruction]) = {
    // Allocate memory
    ins += MovInstr(R0, Immediate(size))
    ins += BranchLinkInstr(MallocLabel)

    // Give the memory pointer to R12
    ins += MovInstr(R12, R0)                                        
  }

  /* Find variable by name in stateTable to find its location */
  def findVarLoc(identifier: String, stateST: StateTable): Register = {
    stateST.lookUpAll(identifier) match {
      case Some(location: Register) => location
      case _ => null
    }
  }

  def sizeOfElem(elemType: Type): Int = {
    elemType match {
      case IntType() => 4
      case BoolType() => 1
      case CharType() => 1
      case PairType(_, _) => 4
      case ArrayType(_) => 4
      case _ => 0
    }
  }

  private def translateDeclare(ident: Ident, 
                               initValue: Rvalue)(
                               implicit st: SymbolTable,
                                        stateST: StateTable,
                                        ins: ListBuffer[Instruction]) = {
    initValue match {
      case initValue: Expr     => translateExpr(initValue)
      case initValue: ArrayLit => declareArrayLit(initValue)
      case initValue: NewPair  => declareNewPair(initValue)
      case initValue: PairElem => declarePairElem(initValue)
      case initValue: Call     => translateCall(initValue)
    }

    // Pop result
    ins += PopInstr(Seq(R8))
    
    // Move to the first available register
    // Assume R4 for now
    ins += MovInstr(R4, R8)

    // Add the location of variable to stateTable
    stateST.add(ident.name, R4)
  }

  private def declareArrayLit(arrayValue: ArrayLit)(
                              implicit st: SymbolTable,
                                       stateST: StateTable,
                                       ins: ListBuffer[Instruction]) = {
    val arr_len = arrayValue.values.size
    // Array store len + 1 elems -> +1 for store length
    val arr_size = (arr_len + 1) * 4

    // malloc array
    // @ (arr-len) element array
    translateMalloc(arr_size)

    // @ array pointers are shifted forwards by 4 bytes (to account for size)
    // move array pointer to a[1]
    ins += AddInstr(R12, R12, Immediate(4))

    // store length of array in a[0]
    ins += MovInstr(R8, Immediate(arr_len))
    ins += StoreInstr(R8, RegOffset(R12, -4))

    // For loop to store each element
    for (i <- 0 until arr_size by 4){
      // Load elem into R8
      translateExpr(arrayValue.values(i))

      // Pop result
      ins += PopInstr(Seq(R8))

      // Store elem to a[i]
      ins += StoreInstr(R8, RegOffset(R12, i))
    }

    // Push Array pointer
    ins += PushInstr(Seq(R12))
  }

  private def declareNewPair(pairValue: NewPair)(
                             implicit st: SymbolTable,
                                      stateST: StateTable,
                                      ins: ListBuffer[Instruction]) = {
    storePairElem(pairValue.expr1)
    storePairElem(pairValue.expr2)
  
    // Allocate for pair
    translateMalloc(8)

    // Pop second elem from stack
    ins += PopInstr(Seq(R8))
    // Store second elem
    ins += StoreInstr(R8, RegOffset(R12, 4))

    // Pop first elem from stack
    ins += PopInstr(Seq(R8))
    // Store first elem
    ins += StoreInstr(R8, RegOffset(R12, 0))

    // Push pair pointer
    ins += PushInstr(Seq(R12))
  }

  private def storePairElem(elem: Expr)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 ins: ListBuffer[Instruction]) = {

    val _type = checkExprType(elem)
    val size = sizeOfElem(_type)

    // If size == 0 -> pair elem is null, do not store
    if (size != 0) {
      translateMalloc(size)

      // Load value of elem
      translateExpr(elem)
      // Pop result
      ins += PopInstr(Seq(R8))

      // store value of elem
      ins += StoreInstr(R8, RegOffset(R12, 0))

      // Push Pair elem pointer
      ins += PushInstr(Seq(R12))
    }
  }

  private def declarePairElem(pairValue: PairElem)(
                              implicit st: SymbolTable,
                                       stateST: StateTable,
                                       ins: ListBuffer[Instruction]): Unit = {
    pairValue.lvalue match {
      case lvalue: Ident     => translateExpr(lvalue)
      case lvalue: ArrayElem => loadArrayElem(lvalue)
      case lvalue: PairElem  => declarePairElem(lvalue)
    }

    // Pop result
    ins += PopInstr(Seq(R8))

    // Check null for pair
    ins += CmpInstr(R8, Immediate(0))
    ins += CondBranchLinkInstr(EqCond, CheckNull)

    // Move fst/snd of pair to R8
    pairValue.index match {
      case "fst" => ins += LoadInstr(R8, RegOffset(R8, 0))
      case "snd" => ins += LoadInstr(R8, RegOffset(R8, 4))
    }

    // Push Pair pointer
    ins += PushInstr(Seq(R8))
  }

  /* Special convention for arrLoad
     R3: Array pointer
     R10: Index
     R14: General purpose */
 def loadArrayElem(arrayValue: ArrayElem)(
                                 implicit st: SymbolTable,
                                          stateST: StateTable,
                                          ins: ListBuffer[Instruction]) = {

    // find array pointer
    var array_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {

      // Move array pointer to R3
      ins += MovInstr(R3, array_loc)
      
      // Pop index to R10
      translateExpr(i)
      ins += PopInstr(Seq(R10))

      // branch to array load
      ins += BranchLinkInstr(ArrayLoad)

      // update array pointer
      array_loc = R3
    }
  }

  /* Special convention for arrStore
     R3: Array pointer
     R10: Index
     R8: value
     R14: General purpose
     Return to R3 */
  // Now only find example of 1 dimension array assign
  private def storeArrayElem(arrayValue: ArrayElem)(
                             implicit st: SymbolTable,
                                      stateST: StateTable,
                                      ins: ListBuffer[Instruction]) = {

    // New value on stack
    
    // Find array pointer
    var array_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {

      translateExpr(i)
      // Pop index into R10
      ins += PopInstr(Seq(R10))

      // Pop assign value into R8
      ins += PopInstr(Seq(R8))

      // Move array pointer to R3
      ins += MovInstr(R3, array_loc)

      // arrayStore - 4 Byte; arrayStoreB - 1 Byte
      val _type = checkLvalueType(arrayValue)
      val storeBranchName = 
      sizeOfElem(_type) match {
        case 1 => ArrayStoreB
        case 4 => ArrayStore
      }

      // branch to array load
      ins += BranchLinkInstr(storeBranchName)

      // update array pointer
      array_loc = R3
    }
  }

  private def translateCall(callValue: Call)(
                            implicit st: SymbolTable,
                                     stateST: StateTable,
                                     ins: ListBuffer[Instruction]) = {
    // Store parameter
    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val para_len = callValue.args.size
    var index = 0

    while (index < para_len) {

      // Pop parameter to R8
      translateExpr(callValue.args(index))
      ins += PopInstr(Seq(R8))

      // First 3 parameters
      if (index < 3) {

        // Change it later, should have pool of usable register
        val reg = 
          index match {
            case 0 => R0
            case 1 => R1
            case 2 => R2
          }

        ins += MovInstr(reg, R8)
      } else {
        // More parameters
        // Push R8
        ins += StoreInstr(R8, RegOffset(SP, -4))
      }

      index += 1
    }

    // Create branch jump
    val branchName = "wacc_" + callValue.ident.name
    ins += BranchInstr(new Label(branchName))

    // Push function return value
    ins += PushInstr(Seq(R0))
  }

  private def translateAssign(target: Lvalue, 
                              newValue: Rvalue)(
                              implicit st: SymbolTable,
                                       stateST: StateTable,
                                       ins: ListBuffer[Instruction]) = {                                          
    newValue match {
      case initValue: Expr     => translateExpr(initValue)
      case initValue: ArrayLit => declareArrayLit(initValue)
      case initValue: NewPair  => declareNewPair(initValue)
      case initValue: PairElem => declarePairElem(initValue)
      case initValue: Call     => translateCall(initValue)
    }

    // New value on stack
    
    target match {
      case target: Ident => {
        // Pop assign value to R8
        ins += PopInstr(Seq(R8))

        // Move assign value to target_loc
        val target_loc = findVarLoc(target.name, stateST)
        ins += MovInstr(target_loc, R8)
      }
      case target: ArrayElem => storeArrayElem(target)
      case target: PairElem => declarePairElem(target)
    }
    
  }

  /* Exit will return value in R0 */
  private def translateExit(expr: Expr)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 ins: ListBuffer[Instruction]) = {
    translateExpr(expr)

    // Pop result to R0 for exit
    ins += PopInstr(Seq(R0))

    ins += BranchLinkInstr(ExitLabel)
  }

  /* Return will return value in R0 */
  private def translateReturn(expr: Expr)(implicit st: SymbolTable,
                                                   stateST: StateTable,
                                                   ins: ListBuffer[Instruction]) = {
    translateExpr(expr)

    // Pop result to R0 for return
    ins += PopInstr(Seq(R0))
  }

  /* Print will print value in R0 */
  private def translatePrint(expr: Expr)(implicit st: SymbolTable,
                                                  stateST: StateTable,
                                                  ins: ListBuffer[Instruction]) = {
    translateExpr(expr)
    // Pop result to R0 for print
    ins += PopInstr(Seq(R0))

    val _type = checkExprType(expr)
    val printType = _type match {
      case IntType()  => PrintInt
      case BoolType() => PrintBool
      case CharType() => PrintChar
      case StrType()  => PrintStr
      case PairType(_, _) => PrintPointer
      case ArrayType(_) => PrintPointer
      case _ => null
    }

    ins += BranchLinkInstr(printType)
  }

  /* Println will print value in R0 */
  private def translatePrintln(expr: Expr)(implicit st: SymbolTable,
                                                    stateST: StateTable,
                                                    ins: ListBuffer[Instruction]) = {
    translatePrint(expr)
    ins += BranchLinkInstr(PrintLine)
  }

  /* Read will read value to R0 */
  private def translateRead(lvalue: Lvalue)(implicit st: SymbolTable,
                                                     stateST: StateTable,
                                                     ins: ListBuffer[Instruction]) = {

    val _type = checkLvalueType(lvalue)
    val readType = _type match {
      case IntType() => ReadInt
      case CharType() => ReadChar
      case _ => null
    }

    lvalue match {
      case lvalue: Ident => {

        translateExpr(lvalue)
        // Pop original value to R0
        ins += PopInstr(Seq(R0))

        // Read from input
        ins += BranchLinkInstr(readType)

        // Move read value from R0 back to target_loc
        val target_loc = findVarLoc(lvalue.name, stateST)
        ins += MovInstr(target_loc, R0)
      }
      case lvalue: ArrayElem => {

        translateExpr(lvalue)
        // Pop original value to R0
        ins += PopInstr(Seq(R0))

        // Read from input
        ins += BranchLinkInstr(readType)

        // Push read value from R0
        ins += PushInstr(Seq(R0))

        // Store read value in ArrayElem
        storeArrayElem(lvalue)
      }
      case lvalue: PairElem => declarePairElem(lvalue)
    }

  }

  /* Free will free value in R0 */
  /* Free can be appied to array and pair */
  private def translateFree(expr: Expr)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 ins: ListBuffer[Instruction]) = {
    // can also be other?
    val location = expr match {
      case expr: Ident => findVarLoc(expr.name, stateST)
      case _ => null
    }

    val _type = checkExprType(expr)
    val freeType = _type match {
      case PairType(_, _) => FreePair
      case ArrayType(_) => FreeLabel
      case _ => null
    }
    
    /* Move pointer to r0 */
    ins += MovInstr(R0, location)

    /* Jump to free */
    ins += BranchLinkInstr(freeType)
  }

  /* New scope will have new state table */
  private def translateIf(expr: Expr, 
                          stats1: List[Stat], 
                          stats2: List[Stat])(implicit st: SymbolTable,
                                                       stateST: StateTable,
                                                       ins: ListBuffer[Instruction]) = {
    // Translate condition
    translateExpr(expr)

    // Pop condition -> not needed but empty stack
    ins += PopInstr(Seq(R8))

    // Allocate new branch name
    val branch_0 = JumpLabel(s"$branchCounter")
    branchCounter += 1

    val branch_1 = JumpLabel(s"$branchCounter")
    branchCounter += 1

    // if true, branch to stat1 (if true branch)
    ins += CondBranchInstr(checkCondCode(expr), branch_0)

    // Somewhere create Branch1 later --> translate stat1 there

    // if false, continue executing stat2 (else branch)
    /* Create new state table */
    val new_stateST2 = new StateTable(stateST)
    stats2.foreach(s => translateStatement(s)(s.symb, new_stateST2, ins))

    // As Branch1 will be below, when false, skip to Branch 2 (will be the rest of code)
    ins += BranchInstr(branch_1)

    // Translate branch1
    // .L0:
    ins += CreateLabel(branch_0)

    // Execute stat1
    val new_stateST1 = new StateTable(stateST)
    stats2.foreach(s => translateStatement(s)(s.symb, new_stateST1, ins))

    // The rest of code needs to be in branch2
    // .L1:
    ins += CreateLabel(branch_1)
  }

  /* New scope will have new state table */
  private def translateWhile(expr: Expr, 
                             stats: List[Stat])(implicit st: SymbolTable,
                                                         stateST: StateTable,
                                                         ins: ListBuffer[Instruction]) = {
    // Allocate new branch name
    val branch_0 = JumpLabel(s"$branchCounter")
    branchCounter += 1

    val branch_1 = JumpLabel(s"$branchCounter")
    branchCounter += 1


    // First, unconditionally jump to Branch 1
    ins += BranchInstr(branch_0)
    
    // Translate Branch 2 here
    // .L1:
    ins += CreateLabel(branch_1)
    val new_stateST = new StateTable(stateST)
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ins))

    // .L0:
    // Translate condition
    ins += CreateLabel(branch_0)
    translateExpr(expr)

    // Pop condition -> not needed but empty stack
    ins += PopInstr(Seq(R8))

    // may need to specially check when expr: bool

    // If condition is true, jump back to branch 2
    ins += CondBranchInstr(checkCondCode(expr), branch_1)

    // Rest of the code goes here
  }

  private def checkCondCode(cond: Expr): CondCode = {
    cond match {
      case BoolLit(_) => EqCond
      case Eq(_, _) => EqCond
      case Neq(_, _) => NeqCond
      case Gt(_, _) => GtCond
      case Gte(_, _) => GteCond
      case Lt(_, _) => LtCond
      case Lte(_, _) => LteCond
      case _ => null
    }
  }

  /* New scope will have new state table */
  private def translateBegin(stats: List[Stat])(implicit stateST: StateTable,
                                                         ins: ListBuffer[Instruction]) = {
    /* Create new state table */
    val new_stateST = new StateTable(stateST)
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ins))
  }
}
