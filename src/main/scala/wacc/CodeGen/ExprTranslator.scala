package wacc.CodeGen


import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.Instructions._

import StatTranslator._
import IR._

object ExprTranslator {
  def translateExpr(
      expr: Expr
    )(implicit st: SymbolTable, 
							 stateST: StateTable,
               ir: IR): Unit = {

    expr match {
      case Add(expr1, expr2)   => translateAdd(expr1, expr2)
      case Sub(expr1, expr2)   => translateSub(expr1, expr2)
      case Mul(expr1, expr2)   => translateMul(expr1, expr2)
      case Div(expr1, expr2)   => translateDiv(expr1, expr2)
      case Mod(expr1, expr2)   => translateMod(expr1, expr2)

      case Gt(expr1, expr2)    => translateCmp(expr1, expr2, GtCond, LteCond)
      case Gte(expr1, expr2)   => translateCmp(expr1, expr2, GteCond, LtCond)
      case Lt(expr1, expr2)    => translateCmp(expr1, expr2, LtCond, GteCond)
      case Lte(expr1, expr2)   => translateCmp(expr1, expr2, LteCond, GtCond)
      case Eq(expr1, expr2)    => translateCmp(expr1, expr2, EqCond, NeqCond)
      case Neq(expr1, expr2)   => translateCmp(expr1, expr2, NeqCond, EqCond)

      case And(expr1, expr2)   => translateAnd(expr1, expr2)  // need to be further considered
      case Or(expr1, expr2)    => translateOr(expr1, expr2)   // need to be further considered
      case Not(expr)           => translateNot(expr)
      case Neg(expr)           => translateNeg(expr)
      case Len(expr)           => translateLen(expr)
      case Ord(expr)           => translateExpr(expr)
      case Chr(expr)           => translateChr(expr)

      case IntLit(value)       => translateInt(value)
      case BoolLit(value)      => translateBool(value)
      case CharLit(value)      => translateChar(value)
      case StrLit(value)       => translateStr(value)
      case PairLit()           => translatePairLit()
      case Ident(name)         => translateIdent(name)
      case expr: ArrayElem     => loadArrayElem(expr)  // need to be further considered
    }
  }

  /* Assume Move to R8 */
  private def translateInt(value: Int)(implicit ir: IR) = {
    if (value >= 0) {
      addInstr(MovInstr(R8, Immediate(value)))
    } else {
      // use load for negative value
      addInstr(LoadInstr(R8, Immediate(value)))
    }
    
    addInstr(PushInstr(Seq(R8)))
  }

  /* Assume Move to R8 */
  private def translateBool(value: Boolean)(implicit ir: IR) = {
    value match {
      case true  => addInstr(MovInstr(R8, Immediate(1)))
      case false => addInstr(MovInstr(R8, Immediate(0)))
    }

    addInstr(PushInstr(Seq(R8)))
  }

  /* Assume Move to R8 */
  private def translateChar(value: Char)(implicit ir: IR) = {
    addInstr(MovInstr(R8, Immediate(value.toInt)))
    
    addInstr(PushInstr(Seq(R8)))
  }

  /* Assume Move to R8 */
  private def translatePairLit()(implicit ir: IR) = {
    // null --> #0
    addInstr(MovInstr(R8, Immediate(0)))

    addInstr(PushInstr(Seq(R8)))
  }

  /* Assume Move to R8 */
  private def translateIdent(name: String)(
                             implicit ir: IR, 
                                      stateST: StateTable) = {
    val loc = findVarLoc(name, stateST)

    loc match {
      case loc: Register  => addInstr(MovInstr(R8, loc))
      case loc: RegIntOffset => addInstr(LoadInstr(R8, loc))
    }

    addInstr(PushInstr(Seq(R8)))
  }

  /* Assume Move to R8 */
  private def translateStr(value: String)(implicit ir: IR) = {
    // add str to constant pool
    addStrConst(value)

    val strId = ir.strConsts.length

    LoadInstr(R8, StrLabel(s"str$strId", value))

    addInstr(PushInstr(Seq(R8)))
  }

  private def translateAdd(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr1 store in R8
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R8)))

    // Expr2 store in R9
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R9)))

    // Result store in R8
    addInstr(AddInstr(R8, R8, R9))

    // Check overflow
    translateCondBLink(VsCond, CheckOverflow)

    // Push Result
    addInstr(PushInstr(Seq(R8)))
  }

	private def translateSub(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr1 store in R8
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R8)))

    // Expr2 store in R9
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R9)))

    // Result store in R8
    addInstr(SubInstr(R8, R8, R9))

    // Check overflow
    translateCondBLink(VsCond, CheckOverflow)   

    // Push Result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateMul(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr1 store in R8
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R8)))

    // Expr2 store in R9
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R9)))

    // Result store in R8
    addInstr( MulInstr(R8, R9, R8, R9))

    // Check overflow
    // Need to modify here. Use constant to define ASR #31
    addInstr(CmpInstr(R9, RegIntOffset(R8, 0)))
    translateCondBLink(NeqCond, CheckOverflow)

    // Push Result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateDiv(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr1 store in R0
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R0)))

    // Expr2 store in R1
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R1)))

    // Check not div by 0
    addInstr(CmpInstr(R1, Immediate(0)))
    translateCondBLink(EqCond, CheckDivZero)

    // Perform division
    translateBLink(DivisionLabel)

    // Push result
    addInstr(PushInstr(Seq(R0)))
  }

  private def translateMod(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr1 store in R0
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R0)))

    // Expr2 store in R1
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R1)))

    // Check not div by 0
    addInstr(CmpInstr(R1, Immediate(0)))
    translateCondBLink(EqCond, CheckDivZero)

    // Perform division
    translateBLink(DivisionLabel)

    // Push result
    addInstr(PushInstr(Seq(R1)))
  }

  private def translateCmp(expr1: Expr, 
                           expr2: Expr,
                           trueCode: CondCode,
                           falseCode: CondCode)(
                           implicit st: SymbolTable, 
                                    stateST: StateTable,
                                    ir: IR) = {
    // Expr1 store in R8
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R8)))

    // Expr2 store in R9
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R9)))

    // Compare expr1, expr2
		addInstr(CmpInstr(R8, R9))

    // Two checker
    addInstr(CondMovInstr(trueCode, R8, Immediate(1)))
    addInstr(CondMovInstr(falseCode, R8, Immediate(0)))

    // Do not need to push for compare?
    addInstr(PushInstr(Seq(R8)))
  }


  private def translateAnd(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr1 store in R8
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R8)))
    
    // Check if expr1 is true
    addInstr(CmpInstr(R8, Immediate(1)))

    // Allocate new branch name
    val branch_0 = JumpLabel(".L" + branchCounter)
    branchCounter += 1

    // If expr1 false, shortcut to L0
    addInstr(CondBranchInstr(NeqCond, branch_0))

    // Expr2 store in R9
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R9)))

    // Check if expr2 is true
    addInstr(CmpInstr(R9, Immediate(1)))

    // .L0:
    addInstr(CreateLabel(branch_0))
    
    // If both true, true
    addInstr(CondMovInstr(EqCond, R8, Immediate(1))) // R8 here should be loc2, but ref compiler used r8
    // If one of it false, false
    addInstr(CondMovInstr(NeqCond, R8, Immediate(0))) // R8 here should be loc2, but ref compiler used r8

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateOr(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {
    // Expr1 store in R8
    translateExpr(expr1)
    addInstr(PopInstr(Seq(R8)))
    
    // Check if expr1 is true
    addInstr(CmpInstr(R8, Immediate(1)))

    // Allocate new branch name
    val branch_0 = JumpLabel(".L" + branchCounter)
    branchCounter += 1

    // If expr1 true, shortcut to L0
    addInstr(CondBranchInstr(EqCond, branch_0))

    // Expr2 store in R9
    translateExpr(expr2)
    addInstr(PopInstr(Seq(R9)))

    // Check if expr2 is true
    addInstr(CmpInstr(R9, Immediate(1)))

    // .L0:
    addInstr(CreateLabel(branch_0))

    
    // If either true, true
    addInstr(CondMovInstr(EqCond, R8, Immediate(1))) // R8 here should be loc2, but ref compiler used r8
    // If both false, false
    addInstr(CondMovInstr(NeqCond, R8, Immediate(0))) // R8 here should be loc2, but ref compiler used r8

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateNot(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {
    // Expr store in R8
    translateExpr(expr)
    addInstr(PopInstr(Seq(R8)))

    // Check expr true or false
		addInstr(CmpInstr(R8, Immediate(1)))
    addInstr(CondMovInstr(NeqCond, R8, Immediate(1)))
    addInstr(CondMovInstr(EqCond, R8, Immediate(0)))

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateNeg(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {

    // Expr store in R8
    translateExpr(expr)
    addInstr(PopInstr(Seq(R8)))

    addInstr(RsbsInstr(R8, R8, Immediate(0)))
    translateCondBLink(VsCond, CheckOverflow)

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateLen(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {
    // Expr store in R8
    translateExpr(expr)
    addInstr(PopInstr(Seq(R8)))

    addInstr(LoadInstr(R8, RegIntOffset(R8, -4)))  // load from a[0] → len a

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

  private def translateChr(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ir: IR) = {
    // Expr store in R8
    translateExpr(expr)
    addInstr(PopInstr(Seq(R8)))

    // Translate to char ASCII
    addInstr(AndInstr(R8, R8, Immediate(127)))

    // Push result
    addInstr(PushInstr(Seq(R8)))
  }

}
