package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes._
import wacc.Instructions._

import StatTranslator._
import IRBuilder._
import Utils._

object ExprTranslator {

  private final val TrueImm = Immediate(1)
  private final val FalseImm = Immediate(0)
  private final val NullImm = Immediate(0)
  private final val ChrImm = Immediate(127)
  private final val PtrSize = 4
  private final val CharSize = 1
  private final val IntSize = 4

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

      case And(expr1, expr2) => translateAnd(expr1, expr2)
      case Or(expr1, expr2)  => translateOr(expr1, expr2)
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

    loc match {
      case loc: Register => addInstr(MovInstr(OpR1, loc))
      case _             => {
        val size = sizeOfElem(checkExprType(ident))
        size match {
          case CharSize => addInstr(LoadSignedByteInstr(OpR1, loc))
          case _        => addInstr(LoadInstr(OpR1, loc))
        }
      }
    }
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
    addInstr(CmpInstr(OpR2, OpR1, Some(ASR(31)))) //TODO: What is 31?
    translateCondBLink(NeqCond, CheckOverflow)
  }

  private def translateDiv(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Div calling convention - store in R0 and R1
    translateTwoExprTo(expr1, expr2, R0, R1)

    // Division by 0 check
    addInstr(CmpInstr(R1, Immediate(0)))
    translateCondBLink(EqCond, CheckDivZero)

    // Perform division
    translateBLink(DivisionLabel)

    // Move result to OpR1 for push
    addInstr(MovInstr(OpR1, R0))
  }

  private def translateMod(
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

    // Move result to OpR1 for push
    addInstr(MovInstr(OpR1, R1))
  }

  private def translateCmp(expr1: Expr, expr2: Expr, trueCode: CondCode, falseCode: CondCode)(implicit
      st: SymbolTable,
      stateST: StateTable,
      ir: IRBuilder
  ) = {

    // Translate Expr1 to OpR1, Expr2 to OpR2
    translateTwoExprTo(expr1, expr2, OpR1, OpR2)

    // Compare expr1, expr2
    addInstr(CmpInstr(OpR1, OpR2))

    // Two checker
    addInstr(CondMovInstr(trueCode, OpR1, TrueImm))
    addInstr(CondMovInstr(falseCode, OpR1, FalseImm))
  }

  private def translateAnd(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {

    // Expr1 store in OpR1
    translateExprTo(expr1, OpR1)

    // Check if expr1 is true
    addInstr(CmpInstr(OpR1, TrueImm))

    // Allocate new branch name
    val branch_0 = JumpLabel(s"${getBranchCounter()}")
    incBranchCounter()

    // If expr1 false, shortcut to L0
    addInstr(CondBranchInstr(NeqCond, branch_0))

    // Expr2 store in OpR2
    translateExprTo(expr2, OpR2)

    // Check if expr2 is true
    addInstr(CmpInstr(OpR2, TrueImm))

    // .L0:
    addInstr(CreateLabel(branch_0))

    // If both true, true
    addInstr(CondMovInstr(EqCond, OpR1, TrueImm)) // R8 here should be loc2, but ref compiler used r8
    // If one of it false, false
    addInstr(CondMovInstr(NeqCond, OpR1, FalseImm)) // R8 here should be loc2, but ref compiler used r8
  }

  private def translateOr(
      expr1: Expr,
      expr2: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr1 store in OpR1
    translateExprTo(expr1, OpR1)

    // Check if expr1 is true
    addInstr(CmpInstr(OpR1, TrueImm))

    // Allocate new branch name
    val branch_0 = JumpLabel(s"${getBranchCounter()}")
    incBranchCounter()

    // If expr1 true, shortcut to L0
    addInstr(CondBranchInstr(EqCond, branch_0))

    // Expr2 store in OpR2
    translateExprTo(expr2, OpR2)

    // Check if expr2 is true
    addInstr(CmpInstr(OpR2, TrueImm))

    // .L0:
    addInstr(CreateLabel(branch_0))

    // If either true, true
    addInstr(CondMovInstr(EqCond, OpR1, TrueImm)) // R8 here should be loc2, but ref compiler used r8
    // If both false, false
    addInstr(CondMovInstr(NeqCond, OpR1, FalseImm)) // R8 here should be loc2, but ref compiler used r8
  }

  private def translateNot(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    // Check expr true or false
    addInstr(CmpInstr(OpR1, TrueImm))
    addInstr(CondMovInstr(NeqCond, OpR1, TrueImm))
    addInstr(CondMovInstr(EqCond, OpR1, FalseImm))
  }

  private def translateNeg(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    addInstr(RsbsInstr(OpR1, OpR1, Immediate(0)))
    translateCondBLink(VsCond, CheckOverflow)
  }

  private def translateLen(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    addInstr(LoadInstr(OpR1, RegIntOffset(OpR1, -PtrSize))) // load from a[0] → len a
  }

  private def translateChr(
      expr: Expr
  )(implicit st: SymbolTable, stateST: StateTable, ir: IRBuilder) = {
    // Expr store in OpR1
    translateExprTo(expr, OpR1)

    // Translate to char ASCII
    addInstr(AndInstr(OpR1, OpR1, ChrImm))
  }

}
