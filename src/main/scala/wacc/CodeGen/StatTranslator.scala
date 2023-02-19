package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.Instructions._
import wacc.SemanticChecker.SymbolObjectType._
import wacc.SemanticChecker.SemanticTypes._
import wacc.SemanticChecker.ExprSemantic.checkExpr
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
      case If(expr, stat1, stat2)           => ???
      case While(expr, stat)                => ???
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
                               initValue: Rvalue)(implicit st: SymbolTable,
                                                           stateST: StateTable,
                                                           instrs: ListBuffer[Instruction]) = {
    initValue match {
      case initValue: Expr => translateExpr(initValue)
      case initValue: ArrayLit => declareArray(initValue.values)
      case initValue: NewPair => declarePair(initValue.expr1, initValue.expr2)
      case initValue: PairElem => translatePairElem(initValue.index, initValue.lvalue)
      case initValue: Call => 
    }
    
    // Move to the first available register
    // Assume R4 for now
    instrs += MovInstr(R4, R8)

    // Add the location of variable to stateTable
    stateST.add(ident.name, R4)
  }

  private def declareArray(elems: List[Expr])(implicit st: SymbolTable,
                                                       stateST: StateTable,
                                                       instrs: ListBuffer[Instruction]) = {
    val arr_len = elems.size
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
      translateExpr(elems(i))

      // Store elem to a[i]
      instrs += StoreInstr(R8, RegOffset(R12, i))
    }

    // Store array pointer back to R8
    // Do this because translateDeclare will Move R8 to R4 at last
    moveToR8(R12)
  
  }

  private def declarePair(elem1: Expr, 
                          elem2: Expr)(implicit st: SymbolTable,
                                                stateST: StateTable,
                                                instrs: ListBuffer[Instruction]) = {
    storePairElem(elem1)
    storePairElem(elem2)
  
    // Allocate for pair
    translateMalloc(8)

    // Pop second elem from stack
    PopInstr(Seq(R8))
    // Store second elem
    instrs += StoreInstr(R8, RegOffset(R12, 4))

    // Pop first elem from stack
    PopInstr(Seq(R8))
    // Store first elem
    instrs += StoreInstr(R8, RegOffset(R12, 0))

    // Store pair pointer back to R8
    // Do this because translateDeclare will Move R8 to R4 at last
    moveToR8(R12)
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
      PushInstr(Seq(R8))
    }
  }

  private def translatePairElem(index: String,
                                lvalue: Lvalue)(implicit st: SymbolTable,
                                                         stateST: StateTable,
                                                         instrs: ListBuffer[Instruction]) = {
    var location: Register = null
    lvalue match {
      case lvalue: Ident => location = findVarLoc(lvalue.name, stateST)
      case lvalue: ArrayElem => 
      case lvalue: PairElem => 
    }

    // Check null for pair
    instrs += CmpInstr(location, Immediate(0))
    instrs += CondBranchLinkInstr(EqCond, CheckNull)
  
  }

  private def translateAssign(target: Lvalue, 
                              newValue: Rvalue)(implicit st: SymbolTable,
                                                         stateST: StateTable,
                                                         instrs: ListBuffer[Instruction]) = {
    var location: Register = null
    target match {
      case target: Ident => location = findVarLoc(target.name, stateST)
      case target: ArrayElem => 
      case target: PairElem => 
    }
                                                            
    newValue match {
      case initValue: Expr => translateExpr(initValue)
      case initValue: ArrayLit => 
      case initValue: NewPair => 
      case initValue: PairElem => 
      case initValue: Call => 
    }
    
    // Move to the first available register
    // Assume R4 for now
    instrs += MovInstr(R4, R8)

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
    var printType: BranchLinkName = null
    expr match {
      case expr: Ident  => printType = checkPrintType(expr.name)
      case expr: StrLit => {

        // Load string from .data to R8

        printType = PrintStr
      }
      case _ => 
    }

    instrs += BranchLinkInstr(printType)
  }

  private def checkPrintType(identifier: String)(
                             implicit st: SymbolTable,
                                      stateST: StateTable,
                                      instrs: ListBuffer[Instruction]): BranchLinkName = {
    var printType: BranchLinkName = null

    // look up variable type to find suitable print type
    st.lookUpAll(identifier, VariableType()) match {
      case Some(obj) => obj.getType() match {
        case IntType()  => printType = PrintInt
        case BoolType() => printType = PrintBool
        case CharType() => printType = PrintChar
        case StrType()  => printType = PrintStr
        case PairType(_, _) => printType = PrintPointer
        case ArrayType(_) => printType   = PrintPointer
        case _ =>
      }
      case None =>
    }

    // find the location of content register
    // Move content to R0 for print
    moveRegToR0(findVarLoc(identifier, stateST))

    printType
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
    var location: Register = null
    var readType: BranchLinkName = null

    lvalue match {
      case lvalue: Ident => {
        location = findVarLoc(lvalue.name, stateST)

        /* Check read int or char */
        st.lookUpAll(lvalue.name, VariableType()) match {
          case Some(obj) => obj.getType() match {
            case IntType()  => readType = ReadInt
            case CharType() => readType = ReadChar
            case _ =>
          }
          case None =>
        }
      }
      case lvalue: ArrayElem => 
      case lvalue: PairElem => 
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
    
    var location: Register = null
    var freeType: BranchLinkName = null

    expr match {
      case expr: Ident => {
        location = findVarLoc(expr.name, stateST)
        st.lookUpAll(expr.name, VariableType()) match {
          case Some(obj) => obj.getType() match {
            case PairType(_, _)  => freeType = FreePair
            case ArrayType(_)  => freeType = FreeLabel
            case _ =>
          }
          case _ =>
        }
      }
      case _ =>
    }
    
    /* Move pointer to r0 */
    moveRegToR0(location)

    /* Jump to free */
    instrs += BranchLinkInstr(freeType)
  }

  /* New scope will have new state table */
  private def translateBegin(stats: List[Stat])(implicit st: SymbolTable,
                                                         stateST: StateTable,
                                                         instrs: ListBuffer[Instruction]) = {
    /* Create new state table */
    val new_stateST = new StateTable(stateST)
    stats.foreach(s => translateStatement(s)(st, new_stateST, instrs))
  }
}
