package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes._
import wacc.Instructions._

import StatTranslator._
import IRBuilder._
import Utils._

object ExprTranslator {

  def translateExpr(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder): Unit = {
    expr match {
      case Add(expr1, expr2) => translateAdd(expr1, expr2)
      case Sub(expr1, expr2) => translateSub(expr1, expr2)
      case Mul(expr1, expr2) => translateMul(expr1, expr2)
      case Div(expr1, expr2) => translateDiv(expr1, expr2)
      case Mod(expr1, expr2) => translateMod(expr1, expr2)

      case Gt(expr1, expr2)  => translateCmp(expr1, expr2, GtCond, LteCond)
      case Gte(expr1, expr2) => translateCmp(expr1, expr2, GteCond, LtCond)
      case Lt(expr1, expr2)  => translateCmp(expr1, expr2, LtCond, GteCond)
      case Lte(expr1, expr2) => translateCmp(expr1, expr2, LteCond, GtCond)
      case Eq(expr1, expr2)  => translateCmp(expr1, expr2, EqCond, NeqCond)
      case Neq(expr1, expr2) => translateCmp(expr1, expr2, NeqCond, EqCond)

      case And(expr1, expr2) => translateAndOr(expr1, expr2, NeqCond)
      case Or(expr1, expr2)  => translateAndOr(expr1, expr2, EqCond)
      case Not(expr)         => translateNot(expr)
      case Neg(expr)         => translateNeg(expr)
      case Len(expr)         => translateLen(expr)
      case Ord(expr)         => translateExprTo(expr, OpR1)
      case Chr(expr)         => translateChr(expr)

      case IntLit(value)   => translateInt(value)
      case BoolLit(value)  => translateBool(value)
      case CharLit(value)  => translateChar(value)
      case StrLit(value)   => translateStr(value)
      case PairLit()       => translatePairLit()
      case expr: Ident     => translateIdent(expr)
      case expr: ArrayElem => loadArrayElem(expr)
      case expr: StructElem => loadStructElem(expr)
    }
    
    // Result will be load to OpR1, push result
    addInstr(PushInstr(Seq(OpR1)))
  }

  /* Assume Move to R8 */
  private def translateInt(value: Int)(implicit ir: IRBuilder) = {
    if (value >= 0) {
      addInstr(MovInstr(OpR1, Immediate(value)))
    } else {
      // use load for negative value
      addInstr(LoadInstr(OpR1, Immediate(value)))
    }
  }

  /* Assume Move to R8 */
  private def translateBool(value: Boolean)(implicit ir: IRBuilder) = {
    value match {
      case true  => addInstr(MovInstr(OpR1, TrueImm))
      case false => addInstr(MovInstr(OpR1, FalseImm))
    }
  }

  /* Assume Move to R8 */
  private def translateChar(value: Char)(implicit ir: IRBuilder) = {
    addInstr(MovInstr(OpR1, Immediate(value.toInt)))
  }

  /* Assume Move to R8 */
  private def translatePairLit()(implicit ir: IRBuilder) = {
    addInstr(MovInstr(OpR1, NullImm))
  }

  /* Assume Move to R8 */
  private def translateIdent(ident: Ident)(implicit st: SymbolTable, ir: IRBuilder, stateST: StateTable) = {
    val loc = findVarLoc(ident.name, stateST)
    val size = sizeOfElem(checkExprType(ident))
    // Load value from loc
    locMovLoad(size, OpR1, loc)
  }

  /* Assume Move to R8 */
  private def translateStr(value: String)(implicit ir: IRBuilder) = {
    // add str to constant pool
    addStrConst(value)

    // get index of this string
    val strId = findStrConstIndex(value)

    addInstr(LoadInstr(OpR1, StrLabel(s"str$strId", value)))
  }

  private def translateAdd(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Translate Expr1 to OpR1, Expr2 to OpR2
    translateTwoExprTo(expr1, expr2, OpR1, OpR2)

    // Result store in OpR1
    addInstr(AddInstr(OpR1, OpR1, OpR2))

    // Check overflow
    translateCondBLink(VsCond, CheckOverflow)
  }

  private def translateSub(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Translate Expr1 to OpR1, Expr2 to OpR2
    translateTwoExprTo(expr1, expr2, OpR1, OpR2)

    // Result store in OpR1
    addInstr(SubInstr(OpR1, OpR1, OpR2))

    // Check overflow
    translateCondBLink(VsCond, CheckOverflow)
  }

  private def translateMul(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Translate Expr1 to OpR1, Expr2 to OpR2
    translateTwoExprTo(expr1, expr2, OpR1, OpR2)

    // Result store in OpR1
    addInstr(MulInstr(OpR1, OpR2, OpR1, OpR2))

    // Check overflow
    addInstr(CmpInstr(OpR2, OpR1, Some(MulShiftConst)))
    translateCondBLink(NeqCond, CheckOverflow)
  }

  private def translateDiv(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    performDivision(expr1, expr2)

    // Div result store in R0 -> division convention
    addInstr(MovInstr(OpR1, R0))
  }

  private def translateMod(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    performDivision(expr1, expr2)

    // Mod store in R1 -> division convention
    addInstr(MovInstr(OpR1, R1))
  }

  private def translateCmp(expr1: Expr, 
                           expr2: Expr, 
                           trueCode: CondCode, 
                           falseCode: CondCode)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IRBuilder
  ) = {

    // Translate Expr1 to OpR1, Expr2 to OpR2
    translateTwoExprTo(expr1, expr2, OpR1, OpR2)

    // Compare expr1, expr2
    addInstr(CmpInstr(OpR1, OpR2))

    // Mov true/false to OpR1 depending on condition
    condMovTwoChecker(trueCode, falseCode, OpR1)
  }

  private def translateAndOr(
      expr1: Expr,
      expr2: Expr,
      shortCutCode: CondCode
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr1 store in OpR1
    translateExprTo(expr1, OpR1)

    // Check if expr1 is true
    addInstr(CmpInstr(OpR1, TrueImm))

    // Allocate new branch name
    val branch_0 = getJumpLabel()

    // If expr1 false(And)/true(Or), shortcut to L0
    addInstr(CondBranchInstr(shortCutCode, branch_0))

    // Expr2 store in OpR2
    translateExprTo(expr2, OpR2)

    // Check if expr2 is true
    addInstr(CmpInstr(OpR2, TrueImm))

    // .L0:
    addInstr(CreateLabel(branch_0))

    // Mov true/false to OpR1 depending on condition
    condMovTwoChecker(EqCond, NeqCond, OpR1)
    
  }

  private def translateNot(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    // Check expr true or false
    addInstr(CmpInstr(OpR1, TrueImm))

    // Inverse expr use two checker
    condMovTwoChecker(NeqCond, EqCond, OpR1)
  }

  private def translateNeg(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    // Signed sub
    addInstr(RsbsInstr(OpR1, OpR1, Immediate(0)))

    // Check overflow
    translateCondBLink(VsCond, CheckOverflow)
  }

  private def translateLen(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    addInstr(LoadInstr(OpR1, RegIntOffset(OpR1, ArrayLenOffset))) // load from a[0] → len a
  }

  private def translateChr(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    // Translate to char ASCII
    addInstr(AndInstr(OpR1, OpR1, ChrImm))
  }

  private def performDivision(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Div calling convention - store in R0 and R1
    translateTwoExprTo(expr1, expr2, R0, R1)

    // Divison by 0 check
    addInstr(CmpInstr(R1, Immediate(0)))
    translateCondBLink(EqCond, CheckDivZero)

    // Perform division
    translateBLink(DivisionLabel)
  }

  private def condMovTwoChecker(trueCode: CondCode, 
                                falseCode: CondCode, 
                                dest: Register)(implicit ir: IRBuilder) = {
    // Move #1 to dest to represent true                      
    addInstr(CondMovInstr(trueCode, dest, TrueImm))
    // Move #0 to dest to represent true
    addInstr(CondMovInstr(falseCode, dest, FalseImm))
  }

}
