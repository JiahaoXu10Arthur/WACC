package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.Instructions._
import wacc.SemanticChecker.SymbolObjectType._
import wacc.SemanticChecker.SemanticTypes._

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
      case Read(lvalue)                     => ???
      case Free(expr)                       => ???
      case Return(expr)                     => translateReturn(expr)
      case Exit(expr)                       => translateExit(expr)
      case Print(expr)                      => translatePrint(expr)
      case Println(expr)                    => translatePrintln(expr)
      case If(expr, stat1, stat2)           => ???
      case While(expr, stat)                => ???
      case Begin(stat)                      => ???
    }

    instrs.toSeq
  }

  /* Move a specific register to R0 */
  def moveRegToR0(oringinLoc: Register)(implicit st: SymbolTable,
                                                         stateST: StateTable,
                                                         instrs: ListBuffer[Instruction]) = {
    instrs += MovInstr(R0, oringinLoc)                                              
  }

  def moveToR8(content: Operand)(implicit st: SymbolTable,
                                          stateST: StateTable,
                                          instrs: ListBuffer[Instruction]) = {
    instrs += MovInstr(R8, content)                                              
  }

  def findVarLoc(identifier: String, stateST: StateTable): Register = {
    stateST.lookUpAll(identifier) match {
      case Some(location: Register) => location
      case _ => null
    }
  }    
    
  private def translateDeclare(ident: Ident, 
                               initValue: Rvalue)(implicit st: SymbolTable,
                                                           stateST: StateTable,
                                                           instrs: ListBuffer[Instruction]) = {
    initValue match {
      case initValue: Expr => translateExpr(initValue)
      case initValue: ArrayLit => 
      case initValue: NewPair => 
      case initValue: PairElem => 
      case initValue: Call => 
    }
    
    // Move to the first available register
    // Assume R4 for now
    instrs += MovInstr(R4, R8)

    // Add the location of variable to stateTable
    stateST.add(ident.name, R4)
  }

  private def translateAssign (target: Lvalue, 
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

    instrs += ExitInstr()
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
    var printType: PrintType = null
    expr match {
      case expr: Ident  => printType = checkPrintType(expr.name)
      case expr: StrLit => {

        // Load string from .data to R8

        printType = PrintS
      }
      case _ => 
    }

    instrs += PrintInstr(printType)
  }

  private def checkPrintType(identifier: String)(implicit st: SymbolTable,
                                                          stateST: StateTable,
                                                          instrs: ListBuffer[Instruction]): PrintType = {
    var printType: PrintType = null

    // look up variable type to find suitable print type
    st.lookUpAll(identifier, VariableType()) match {
      case Some(obj) => obj.getType() match {
        case IntType()  => printType = PrintI
        case BoolType() => printType = PrintB
        case CharType() => printType = PrintC
        case StrType()  => printType = PrintS
        case PairType(_, _) => printType = PrintP
        case ArrayType(_) => printType   = PrintP
        case _ =>
      }
      case None =>
    }

    // find the location of content register
    moveRegToR0(findVarLoc(identifier, stateST))

    printType
  }

  /* Println will print value in R0 */
  private def translatePrintln(expr: Expr)(implicit st: SymbolTable,
                                                    stateST: StateTable,
                                                    instrs: ListBuffer[Instruction]) = {
    translatePrint(expr)
    instrs += PrintlnInstr()
  }
}
