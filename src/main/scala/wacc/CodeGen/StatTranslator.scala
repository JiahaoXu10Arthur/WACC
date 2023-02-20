package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.Instructions._
import wacc.SemanticChecker.SemanticTypes._
import wacc.SemanticChecker.ExprSemantic.checkExpr
import wacc.SemanticChecker.ValueSemantic.checkLvalue
import wacc.Error.Errors._

import ExprTranslator._

import scala.collection.mutable.ListBuffer


object StatTranslator {
  def translateStatement(
      stat: Stat
    )(implicit st: SymbolTable,
               stateST: StateTable,
               instrs: ListBuffer[Instruction]): Seq[Instruction] = {

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

    instrs.toSeq
  }

  /* Move a specific register to R0 */
  def moveRegToR0(oringinLoc: Register)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 instrs: ListBuffer[Instruction]) = {
    instrs += MovInstr(R0, oringinLoc)                                              
  }

  /* Move an operand to R8 */
  def moveToR8(content: Operand)(implicit st: SymbolTable,
                                          stateST: StateTable,
                                          instrs: ListBuffer[Instruction]) = {
    instrs += MovInstr(R8, content)                                              
  }

  /* Translate for malloc part */
  def translateMalloc(size: Int)(implicit st: SymbolTable,
                                          stateST: StateTable,
                                          instrs: ListBuffer[Instruction]) = {
    // Allocate memory
    instrs += MovInstr(R0, Immediate(size))
    instrs += BranchLinkInstr(MallocLabel)

    // Give the memory pointer to R12
    instrs += MovInstr(R12, R0)                                        
  }

  /* Find variable by name in stateTable to find its location */
  def findVarLoc(identifier: String, stateST: StateTable): Register = {
    stateST.lookUpAll(identifier) match {
      case Some(location: Register) => location
      case _ => null
    }
  }

  private def sizeOfElem(elemType: Type): Int = {
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
                                        instrs: ListBuffer[Instruction]): Register = {
    initValue match {
      case initValue: Expr => translateExpr(initValue)
      case initValue: ArrayLit => declareArrayLit(initValue)
      case initValue: NewPair => declareNewPair(initValue)
      case initValue: PairElem => declarePairElem(initValue)
      case initValue: Call => translateCall(initValue)
    }
    
    // Move to the first available register
    // Assume R4 for now
    instrs += MovInstr(R4, R8)

    // Add the location of variable to stateTable
    stateST.add(ident.name, R4)

    R4
  }

  private def declareArrayLit(arrayValue: ArrayLit)(
                              implicit st: SymbolTable,
                                       stateST: StateTable,
                                       instrs: ListBuffer[Instruction]): Register = {
    val arr_len = arrayValue.values.size
    // Array store len + 1 elems -> +1 for store length
    val arr_size = (arr_len + 1) * 4

    // malloc array
    // @ (arr-len) element array
    translateMalloc(arr_size)

    // @ array pointers are shifted forwards by 4 bytes (to account for size)
    // move array pointer to a[1]
    instrs += AddInstr(R12, R12, Immediate(4))

    // store length of array in a[0]
    moveToR8(Immediate(arr_len))
    instrs += StoreInstr(R8, RegOffset(R12, -4))

    // For loop to store each element
    for (i <- 0 until arr_size by 4){
      // Load elem into R8
      translateExpr(arrayValue.values(i))

      // Store elem to a[i]
      instrs += StoreInstr(R8, RegOffset(R12, i))
    }

    // Store array pointer back to R8
    // Do this because translateDeclare will Move R8 to R4 at last
    moveToR8(R12)

    R8
  }

  private def declareNewPair(pairValue: NewPair)(
                             implicit st: SymbolTable,
                                      stateST: StateTable,
                                      instrs: ListBuffer[Instruction]): Register = {
    storePairElem(pairValue.expr1)
    storePairElem(pairValue.expr2)
  
    // Allocate for pair
    translateMalloc(8)

    // Pop second elem from stack
    instrs += PopInstr(Seq(R8))
    // Store second elem
    instrs += StoreInstr(R8, RegOffset(R12, 4))

    // Pop first elem from stack
    instrs += PopInstr(Seq(R8))
    // Store first elem
    instrs += StoreInstr(R8, RegOffset(R12, 0))

    // Store pair pointer back to R8
    // Do this because translateDeclare will Move R8 to R4 at last
    moveToR8(R12)

    R8
  }

  private def storePairElem(elem: Expr)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 instrs: ListBuffer[Instruction]) = {

    val _type = checkExpr(elem)(st, new ListBuffer[WACCError]())
    val size = sizeOfElem(_type)

    // If size == 0 -> pair elem is null, do not store
    if (size != 0) {
      translateMalloc(size)

      // Load value of elem
      translateExpr(elem)
      instrs += StoreInstr(R8, RegOffset(R12, 0))

      // Move value of elem to R8
      moveToR8(R12)
      // Push R8
      instrs += PushInstr(Seq(R8))
    }
  }

  private def declarePairElem(pairValue: PairElem)(
                              implicit st: SymbolTable,
                                       stateST: StateTable,
                                       instrs: ListBuffer[Instruction]): Register = {
    val location = pairValue.lvalue match {
      case lvalue: Ident => translateIdent(lvalue.name)
      case lvalue: ArrayElem => loadArrayElem(lvalue)
      case lvalue: PairElem => declarePairElem(lvalue)
    }

    // Check null for pair
    instrs += CmpInstr(location, Immediate(0))
    instrs += CondBranchLinkInstr(EqCond, CheckNull)

    // Move fst/snd of pair to R8
    pairValue.index match {
      case "fst" => instrs += LoadInstr(R8, RegOffset(location, 0))
      case "snd" => instrs += LoadInstr(R8, RegOffset(location, 4))
    }

    R8
  }

  /* Special convention for arrLoad
     R3: Array pointer
     R10: Index
     R14: General purpose */
 def loadArrayElem(arrayValue: ArrayElem)(
                                 implicit st: SymbolTable,
                                          stateST: StateTable,
                                          instrs: ListBuffer[Instruction]): Register = {

    // Location of array pointer
    var pointer_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {
      // Move index to R10
      i match {
        case IntLit(value) => instrs += MovInstr(R10, Immediate(value))
        // Should consider pair and array?
        case _ =>
      }

      // Move array pointer to r3
      instrs += MovInstr(R3, pointer_loc)

      // branch to array load
      instrs += BranchLinkInstr(ArrayLoad)

      // Move new array pointer to R8
      moveToR8(R3)
      pointer_loc = R8
    }

    R8
  }

  /* Special convention for arrStore
     R3: Array pointer
     R10: Index
     R8: value
     R14: General purpose 
     Return to R3 */
  // Now only find example of 1 dimension array assign
  private def storeArrayElem(arrayValue: ArrayElem,
                             assign_loc: Operand)(
                             implicit st: SymbolTable,
                                      stateST: StateTable,
                                      instrs: ListBuffer[Instruction]): Register = {

    // Location of array pointer
    val pointer_loc = findVarLoc(arrayValue.ident.name, stateST)

    // For each dimension
    for (i <- arrayValue.index) {
      // Move index into R10
      val index_loc = translateExpr(i)
      instrs += MovInstr(R10, index_loc)

      // Move assign value into R8
      instrs += MovInstr(R8, assign_loc)

      // Move array pointer to r3
      instrs += MovInstr(R3, pointer_loc)

      // branch to array load
      instrs += BranchLinkInstr(ArrayStore)
      // when to use arrayStoreB ?

      // // Move new array pointer to R8
      // moveToR8(R3)
      // pointer_loc = R8

      // Move original array pointer to R8
      moveToR8(pointer_loc)
    }

    R8
  }

  private def translateCall(callValue: Call)(
                            implicit st: SymbolTable,
                                     stateST: StateTable,
                                     instrs: ListBuffer[Instruction]) = {
    // Store parameter
    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val para_len = callValue.args.size
    var index = 0

    while (index < para_len) {

      translateExpr(callValue.args(index))

      // First 3 parameters
      if (index < 3) {

        // Change it later, should have pool of usable register
        val reg = 
          index match {
            case 0 => R0
            case 1 => R1
            case 2 => R2
          }

        instrs += MovInstr(reg, R8)
      } else {
        // More parameters
        // Push R8
        instrs += StoreInstr(R8, RegOffset(SP, -4))
      }

      index += 1
    }

    // Create branch jump
    val branchName = "wacc_" + callValue.ident.name
    instrs += BranchInstr(new Label(branchName))

    // Move function return value to R8
    moveToR8(R0)

    R8
  }

  private def translateAssign(target: Lvalue, 
                              newValue: Rvalue)(
                              implicit st: SymbolTable,
                                       stateST: StateTable,
                                       instrs: ListBuffer[Instruction]) = {
    val value_loc =                                               
      newValue match {
        case initValue: Expr => translateExpr(initValue)
        case initValue: ArrayLit => declareArrayLit(initValue)
        case initValue: NewPair => declareNewPair(initValue)
        case initValue: PairElem => declarePairElem(initValue)
        case initValue: Call => null
      }

    val target_loc = 
      target match {
        case target: Ident => findVarLoc(target.name, stateST)
        case target: ArrayElem => storeArrayElem(target, value_loc)
        case target: PairElem => declarePairElem(target)
      }
    
    // Move value to target
    // May not needed when arrayElem?
    instrs += MovInstr(target_loc, value_loc)
  }

  /* Exit will return value in R0 */
  private def translateExit(expr: Expr)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 instrs: ListBuffer[Instruction]) = {
    translateExpr(expr)

    // assume expr will be read to R8
    moveRegToR0(R8)

    instrs += BranchLinkInstr(ExitLabel)
  }

  /* Return will return value in R0 */
  private def translateReturn(expr: Expr)(implicit st: SymbolTable,
                                                   stateST: StateTable,
                                                   instrs: ListBuffer[Instruction]) = {
    translateExpr(expr)

    // assume expr will be read to R8
    moveRegToR0(R8)
  }

  /* Print will print value in R0 */
  private def translatePrint(expr: Expr)(implicit st: SymbolTable,
                                                  stateST: StateTable,
                                                  instrs: ListBuffer[Instruction]) = {
    // find the location of content register
    val location = translateExpr(expr)

    val _type = checkExpr(expr)(st, new ListBuffer[WACCError]())
    val printType = _type match {
      case IntType()  => PrintInt
      case BoolType() => PrintBool
      case CharType() => PrintChar
      case StrType()  => PrintStr
      case PairType(_, _) => PrintPointer
      case ArrayType(_) => PrintPointer
      case _ => null
    }

    // Move content to R0 for print
    moveRegToR0(location)

    instrs += BranchLinkInstr(printType)
  }

  /* Println will print value in R0 */
  private def translatePrintln(expr: Expr)(implicit st: SymbolTable,
                                                    stateST: StateTable,
                                                    instrs: ListBuffer[Instruction]) = {
    translatePrint(expr)
    instrs += BranchLinkInstr(PrintLine)
  }

  /* Read will read value to R0 */
  private def translateRead(lvalue: Lvalue)(implicit st: SymbolTable,
                                                     stateST: StateTable,
                                                     instrs: ListBuffer[Instruction]) = {

    val location = lvalue match {
      case lvalue: Ident => findVarLoc(lvalue.name, stateST)
      case lvalue: ArrayElem => loadArrayElem(lvalue)
      case lvalue: PairElem => declarePairElem(lvalue)
    }

    val _type = checkLvalue(lvalue)(st, new ListBuffer[WACCError]())
    val readType = _type match {
      case IntType() => ReadInt
      case CharType() => ReadChar
      case _ => null
    }
    
    /* Read original data to r0 */
    moveRegToR0(location)
    /* Read from input */
    instrs += BranchLinkInstr(readType)
    /* store input data to variable */
    instrs += MovInstr(location, R0)
  }

  /* Free will free value in R0 */
  /* Free can be appied to array and pair */
  private def translateFree(expr: Expr)(implicit st: SymbolTable,
                                                 stateST: StateTable,
                                                 instrs: ListBuffer[Instruction]) = {
    translateExpr(expr)

    val location = expr match {
      case expr: Ident => findVarLoc(expr.name, stateST)
      case _ => null
    }

    val _type = checkExpr(expr)(st, new ListBuffer[WACCError]())
    val freeType = _type match {
      case PairType(_, _) => FreePair
      case ArrayType(_) => FreeLabel
      case _ => null
    }
    
    /* Move pointer to r0 */
    moveRegToR0(location)

    /* Jump to free */
    instrs += BranchLinkInstr(freeType)
  }

  /* New scope will have new state table */
  private def translateIf(expr: Expr, 
                          stats1: List[Stat], 
                          stats2: List[Stat])(implicit st: SymbolTable,
                                                       stateST: StateTable,
                                                       instrs: ListBuffer[Instruction]) = {
    // Translate condition
    translateExpr(expr)

    // may need to specially check when expr: bool

    // if true, branch to stat1
    instrs += CondBranchInstr(checkCondCode(expr), new Label(".L0"))

    // Somewhere create Branch1 later --> translate stat1 there

    // if false, continue executing stat2
    /* Create new state table */
    val new_stateST1 = new StateTable(stateST)
    stats2.foreach(s => translateStatement(s)(s.symb, new_stateST1, instrs))

    // As Branch1 will be below, when false, skip to Branch 2 (will be the rest of code)
    instrs += BranchInstr(new Label(".L1"))

    // Translate branch1
    // .L0:
    instrs += JumpLabel(".L0")
    val new_stateST2 = new StateTable(stateST)
    stats2.foreach(s => translateStatement(s)(s.symb, new_stateST2, instrs))

    // The rest of code needs to be in branch2
    // .L1:
    instrs += JumpLabel(".L1")
  }

  /* New scope will have new state table */
  private def translateWhile(expr: Expr, 
                             stats: List[Stat])(implicit st: SymbolTable,
                                                         stateST: StateTable,
                                                         instrs: ListBuffer[Instruction]) = {
    // First, unconditionally jump to Branch 1
    instrs += BranchInstr(new Label(".L0"))
    
    // Translate Branch 2 here
    // .L1:
    instrs += JumpLabel(".L1")
    val new_stateST = new StateTable(stateST)
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, instrs))

    // .L0:
    // Translate condition
    instrs += JumpLabel(".L0")
    translateExpr(expr)

    // may need to specially check when expr: bool

    // If condition is true, jump back to branch 2
    instrs += CondBranchInstr(checkCondCode(expr), new Label(".L1"))

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
                                                         instrs: ListBuffer[Instruction]) = {
    /* Create new state table */
    val new_stateST = new StateTable(stateST)
    stats.foreach(s => translateStatement(s)(s.symb, new_stateST, instrs))
  }
}