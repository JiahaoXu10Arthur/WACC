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
      case Mul(expr1, expr2)   => translateMul(expr1, expr2)
      case Div(expr1, expr2)   => translateDiv(expr1, expr2)
      case Mod(expr1, expr2)   => translateMod(expr1, expr2)
      case Gt(expr1, expr2)    => translateGt(expr1, expr2)
      case Gte(expr1, expr2)   => translateGte(expr1, expr2)
      case Lt(expr1, expr2)    => translateLt(expr1, expr2)
      case Lte(expr1, expr2)   => translateLte(expr1, expr2)
      case Eq(expr1, expr2)    => translateEq(expr1, expr2)
      case And(expr1, expr2)   => translateAnd(expr1, expr2)  // need to be further considered
      case Or(expr1, expr2)    => translateOr(expr1, expr2)  // need to be further considered
      case Not(expr)           => translateNot(expr)
      case Neg(expr)           => translateNeg(expr)
      case Len(expr)           => translateLen(expr)
      case Ord(expr)           => translateOrd(expr)
      case Chr(expr)           => translateChr(expr)

      case IntLit(value)       => translateInt(value)
      case BoolLit(value)      => translateBool(value)
      case CharLit(value)      => translateChar(value)
      case StrLit(value)       => translateStr(value)
      case PairLit()           => translatePairLit()
      // case Ident(name)         => ???
      case ArrayElem(ident, index)   => translateArrayElem(ident, index) // need to be further considered
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

  /* Assume Move to R8 */
  private def translateStr(value: String)(implicit st: SymbolTable, 
                                          ins: ListBuffer[Instruction], 
                                          stateST: StateTable): Register = {
    val strLabel = findLabelByString(value)
    LoadInstr(R8, strLabel)

    R8
  }

  /* Assume Move to R8 */
  private def translateArrayElem(ident: Ident, index: List[Expr])(implicit st: SymbolTable, 
                                          ins: ListBuffer[Instruction], 
                                          stateST: StateTable): Register = {
    

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

  private def translateMul(
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
		ins +=  MulInstr(R8, R9, loc1, loc2)
  }

  private def translateDiv(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    // need to move both expressions to r0 and r1
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

    // Taking value from r0 and r1
		ins += BranchLinkInstr(CheckDivZero)
    ins += BranchLinkInstr(DivBl)
    // Things are stored in r0 and r1 after bl
  }

  private def translateMod(
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

    // Same with Div, but need to move what is stored in r1 to r0 after bl
		ins += BranchLinkInstr(CheckDivZero)
    ins += BranchLinkInstr(DivBl)
    ins += MovInstr(R0, R1)
  }

  private def translateGt(
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

		ins += CmpInstr(loc1, loc2)
    ins += CondMovInstr(GtCond, R8, Immediate(1))
    ins += CondMovInstr(LteCond, R8, Immediate(0))
  }

  private def translateGte(
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

		ins += CmpInstr(loc1, loc2)
    ins += CondMovInstr(GteCond, R8, Immediate(1))
    ins += CondMovInstr(LtCond, R8, Immediate(0))
  }

  private def translateLt(
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

		ins += CmpInstr(loc1, loc2)
    ins += CondMovInstr(LtCond, R8, Immediate(1))
    ins += CondMovInstr(GteCond, R8, Immediate(0))
  }

  private def translateLte(
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

		ins += CmpInstr(loc1, loc2)
    ins += CondMovInstr(LteCond, R8, Immediate(1))
    ins += CondMovInstr(GtCond, R8, Immediate(0))
  }

  private def translateEq(
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

		ins += CmpInstr(loc1, loc2)
    ins += CondMovInstr(EqCond, R8, Immediate(1))
    ins += CondMovInstr(NeqCond, R8, Immediate(0))
  }

  private def translateAnd(
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

		ins += CmpInstr(loc1, Immediate(1))
    ins += CondBranchInstr(NeqCond, ".L0")
    ins += CmpInstr(loc2, Immediate(1))  //Chunky code but appeared on ref compiler
    ins += BranchInstr(".L0")
    ins += CondMovInstr(EqCond, R8, Immediate(1)) // R8 here should be loc2, but ref compiler used r8
    ins += CondMovInstr(NeqCond, R8, Immediate(0)) // R8 here should be loc2, but ref compiler used r8
    // The assembly code on the ref compiler for And is just a piece of shit. Too many unused chunky repetitive code.
  }

  private def translateOr(
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

    // Same with Add. A chunk of bullshit. Need futher discussion
		ins += CmpInstr(loc1, Immediate(1))
    ins += CondBranchInstr(NeqCond, ".L0")
    ins += CmpInstr(loc2, Immediate(1))  //Chunky code but appeared on ref compiler
    ins += BranchInstr(".L0")
    ins += CondMovInstr(EqCond, R8, Immediate(1)) // R8 here should be loc2, but ref compiler used r8
    ins += CondMovInstr(NeqCond, R8, Immediate(0)) // R8 here should be loc2, but ref compiler used r8
    // The assembly code on the ref compiler for And is just a piece of shit. Too many unused chunky repetitive code.
  }

  private def translateNot(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc: Register = null
    expr match {
			case expr1: IntLit => loc = translateInt(expr.value)
			case expr1: Ident  => loc = findVarLoc(expr.name, stateST)
			case _ => translateExpr(expr)
		}

		ins += CmpInstr(loc, Immediate(1))
    ins += CondMovInstr(NeqCond, R8, Immediate(1))
    ins += CondMovInstr(EqCond, R8, Immediate(0))
  }

  private def translateNeg(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc: Register = null
    expr match {
			case expr1: IntLit => loc = translateInt(expr.value)
			case expr1: Ident  => loc = findVarLoc(expr.name, stateST)
			case _ => translateExpr(expr)
		}

    ins += RsbsInstr(R8, loc, Immediate(0))
    ins += BranchLinkInstr(CheckOverflow)
  }

  private def translateLen(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc: Register = null
    expr match {
			case expr1: IntLit => loc = translateInt(expr.value)
			case expr1: Ident  => loc = findVarLoc(expr.name, stateST)
			case _ => translateExpr(expr)
		}

    ins += LoadInstr(R8, LoadInstr(loc, -4))  // load from a[0] → len a
  }

  private def translateOrd(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc: Register = null
    expr match {
			case expr1: IntLit => loc = translateInt(expr.value)
			case expr1: Ident  => loc = findVarLoc(expr.name, stateST)
			case _ => translateExpr(expr)
		}

    ins += MovInstr(R0, loc)
    ins += BranchLinkInstr(PrintInt)
  }

  private def translateChr(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    var loc: Register = null
    expr match {
			case expr1: IntLit => loc = translateInt(expr.value)
			case expr1: Ident  => loc = findVarLoc(expr.name, stateST)
			case _ => translateExpr(expr)
		}

    ins += AndInstr(R8, loc, Immediate(127))
    ins += MovInstr(R0, R8)
    ins += BranchLinkInstr(PrintChar)
  }



}
