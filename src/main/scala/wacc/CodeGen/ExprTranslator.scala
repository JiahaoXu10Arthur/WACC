package wacc.CodeGen


import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.Instructions._

import StatTranslator._

import scala.collection.mutable.ListBuffer

object ExprTranslator {
  def translateExpr(
      expr: Expr
    )(implicit st: SymbolTable, 
							 stateST: StateTable,
               ins: ListBuffer[Instruction]): Seq[Instruction] = {

    expr match {
      case Add(expr1, expr2)   => translateAdd(expr1, expr2)
      case Sub(expr1, expr2)   => translateSub(expr1, expr2)
      case Mul(expr1, expr2)   => ???
      case Div(expr1, expr2)   => ???
      case Mod(expr1, expr2)   => ???
      case Gt(expr1, expr2)    => ???
      case Gte(expr1, expr2)   => ???
      case Lt(expr1, expr2)    => ???
      case Lte(expr1, expr2)   => ???
      case Eq(expr1, expr2)    => ???
      case And(expr1, expr2)   => ???
      case Or(expr1, expr2)    => ???
      case Not(expr)           => ???
      case Neg(expr)           => ???
      case Len(expr)           => ???
      case Ord(expr)           => ???
      case Chr(expr)           => ???

      case IntLit(value)       => translateInt(value)
      case BoolLit(value)      => translateBool(value)
      case CharLit(value)      => translateChar(value)
      case StrLit(value)       => ???
      case PairLit()           => translatePairLit()
      // case Ident(name)         => ???
      case ArrayElem(ident, index)   => ???
      case _ =>
    }

    ins.toSeq
  }

  /* Assume Move to R8 */
  private def translateInt(value: Int)(implicit st: SymbolTable, 
                                                ins: ListBuffer[Instruction], 
                                                stateST: StateTable): Register = {
    moveToR8(Immediate(value))

    R8
  }

  /* Assume Move to R8 */
  private def translateBool(value: Boolean)(implicit st: SymbolTable, 
                                                     ins: ListBuffer[Instruction], 
                                                     stateST: StateTable): Register = {
    value match {
      case true  => moveToR8(Immediate(1))
      case false => moveToR8(Immediate(0))
    }

    R8
  }

  /* Assume Move to R8 */
  private def translateChar(value: Char)(implicit st: SymbolTable, 
                                                     ins: ListBuffer[Instruction], 
                                                     stateST: StateTable): Register = {
    moveToR8(Immediate(value.toInt))
    
    R8
  }

  /* Assume Move to R8 */
  private def translatePairLit()(implicit st: SymbolTable, 
                                          ins: ListBuffer[Instruction], 
                                          stateST: StateTable): Register = {
    // null --> #0
    moveToR8(Immediate(0))

    R8
  }

  private def translateAdd(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc1: Register = null
    expr1 match {
			case expr1: IntLit => loc1 = translateInt(expr1.value)
			case expr1: Ident  => loc1 = findVarLoc(expr1.name, stateST)
			case _ => translateExpr(expr1)
		}

		var loc2: Register = null
		expr2 match {
			case expr2: IntLit => loc2 = translateInt(expr2.value)
			case expr2: Ident  => loc2 = findVarLoc(expr2.name, stateST)
			case _ => translateExpr(expr2)
		}

    // Assume R10 is the temporary result register
		ins +=  AddInstr(R10, loc1, loc2)
  }

	private def translateSub(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc1: Register = null
    expr1 match {
			case expr1: IntLit => loc1 = translateInt(expr1.value)
			case expr1: Ident  => loc1 = findVarLoc(expr1.name, stateST)
			case _ => translateExpr(expr1)
		}

		var loc2: Register = null
		expr2 match {
			case expr2: IntLit => loc2 = translateInt(expr2.value)
			case expr2: Ident  => loc2 = findVarLoc(expr2.name, stateST)
			case _ => translateExpr(expr2)
		}

    // Assume R10 is the temporary result register
		ins +=  SubInstr(R10, loc1, loc2)
  }


}
