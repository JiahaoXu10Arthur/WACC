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
               ins: ListBuffer[Instruction]): Unit = {

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
  private def translateInt(value: Int)(implicit st: SymbolTable, 
                                                ins: ListBuffer[Instruction], 
                                                stateST: StateTable) = {
    if (value >= 0) {
      ins += MovInstr(R8, Immediate(value))
    } else {
      // use load for negative value
      ins += LoadInstr(R8, Immediate(value))
    }
    
    ins += PushInstr(Seq(R8))
  }

  /* Assume Move to R8 */
  private def translateBool(value: Boolean)(implicit st: SymbolTable, 
                                                     ins: ListBuffer[Instruction], 
                                                     stateST: StateTable) = {
    value match {
      case true  => ins += MovInstr(R8, Immediate(1))
      case false => ins += MovInstr(R8, Immediate(0))
    }

    ins += PushInstr(Seq(R8))
  }

  /* Assume Move to R8 */
  private def translateChar(value: Char)(implicit st: SymbolTable, 
                                                     ins: ListBuffer[Instruction], 
                                                     stateST: StateTable) = {
    ins += MovInstr(R8, Immediate(value.toInt))
    
    ins += PushInstr(Seq(R8))
  }

  /* Assume Move to R8 */
  private def translatePairLit()(implicit st: SymbolTable, 
                                          ins: ListBuffer[Instruction], 
                                          stateST: StateTable) = {
    // null --> #0
    ins += MovInstr(R8, Immediate(0))

    ins += PushInstr(Seq(R8))
  }

  /* Assume Move to R8 */
  private def translateIdent(name: String)(
                             implicit st: SymbolTable, 
                                      ins: ListBuffer[Instruction], 
                                      stateST: StateTable) = {
    val loc = findVarLoc(name, stateST)

    loc match {
      case loc: Register  => ins += MovInstr(R8, loc)
      case loc: RegOffset => ins += LoadInstr(R8, loc)
    }

    ins += PushInstr(Seq(R8))
  }

  /* Assume Move to R8 */
  private def translateStr(value: String)(implicit st: SymbolTable, 
                                          ins: ListBuffer[Instruction], 
                                          stateST: StateTable) = {
    // val strLabel = findLabelByString(value)
    LoadInstr(R8, StrLabel(value))

    ins += PushInstr(Seq(R8))
  }

  private def translateAdd(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr1 store in R8
    translateExpr(expr1)
    ins += PopInstr(Seq(R8))

    // Expr2 store in R9
    translateExpr(expr2)
    ins += PopInstr(Seq(R9))

    // Result store in R8
    ins += AddInstr(R8, R8, R9)

    // Check overflow
    ins += CondBranchLinkInstr(VsCond, CheckOverflow) 

    // Push Result
    ins += PushInstr(Seq(R8))
  }

	private def translateSub(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr1 store in R8
    translateExpr(expr1)
    ins += PopInstr(Seq(R8))

    // Expr2 store in R9
    translateExpr(expr2)
    ins += PopInstr(Seq(R9))

    // Result store in R8
    ins += SubInstr(R8, R8, R9)   

    // Check overflow
    ins += CondBranchLinkInstr(VsCond, CheckOverflow)         

    // Push Result
    ins += PushInstr(Seq(R8))
  }

  private def translateMul(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr1 store in R8
    translateExpr(expr1)
    ins += PopInstr(Seq(R8))

    // Expr2 store in R9
    translateExpr(expr2)
    ins += PopInstr(Seq(R9))

    // Result store in R8
    ins +=  MulInstr(R8, R9, R8, R9)

    // Check overflow
    // Need to modify here. Use constant to define ASR #31
    ins += CmpInstr(R9, RegOffset(R8, 0))
    ins += CondBranchLinkInstr(NeqCond, CheckOverflow)

    // Push Result
    ins += PushInstr(Seq(R8))
  }

  private def translateDiv(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr1 store in R0
    translateExpr(expr1)
    ins += PopInstr(Seq(R0))

    // Expr2 store in R1
    translateExpr(expr2)
    ins += PopInstr(Seq(R1))

    // Check not div by 0
    ins += CmpInstr(R1, Immediate(0))
    ins += CondBranchLinkInstr(EqCond, CheckDivZero)

    // Perform division
    ins += BranchLinkInstr(DivisionLabel)

    // Push result
    ins += PushInstr(Seq(R0))
  }

  private def translateMod(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr1 store in R0
    translateExpr(expr1)
    ins += PopInstr(Seq(R0))

    // Expr2 store in R1
    translateExpr(expr2)
    ins += PopInstr(Seq(R1))

    // Check not div by 0
    ins += CmpInstr(R1, Immediate(0))
    ins += CondBranchLinkInstr(EqCond, CheckDivZero)

    // Perform division
    ins += BranchLinkInstr(DivisionLabel)

    // Push result
    ins += PushInstr(Seq(R1))
  }

  private def translateCmp(expr1: Expr, 
                           expr2: Expr,
                           trueCode: CondCode,
                           falseCode: CondCode)(
                           implicit st: SymbolTable, 
                                    stateST: StateTable,
                                    ins: ListBuffer[Instruction]) = {
    // Expr1 store in R8
    translateExpr(expr1)
    ins += PopInstr(Seq(R8))

    // Expr2 store in R9
    translateExpr(expr2)
    ins += PopInstr(Seq(R9))

    // Compare expr1, expr2
		ins += CmpInstr(R8, R9)

    // Two checker
    ins += CondMovInstr(trueCode, R8, Immediate(1))
    ins += CondMovInstr(falseCode, R8, Immediate(0))

    // Do not need to push for compare?
    ins += PushInstr(Seq(R8))
  }


  private def translateAnd(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr1 store in R8
    translateExpr(expr1)
    ins += PopInstr(Seq(R8))
    
    // Check if expr1 is true
    ins += CmpInstr(R8, Immediate(1))

    // Allocate new branch name
    val branch_0 = JumpLabel(".L" + branchCounter)
    branchCounter += 1

    // If expr1 false, shortcut to L0
    ins += CondBranchInstr(NeqCond, branch_0)

    // Expr2 store in R9
    translateExpr(expr2)
    ins += PopInstr(Seq(R9))

    // Check if expr2 is true
    ins += CmpInstr(R9, Immediate(1))

    // .L0:
    ins += CreateLabel(branch_0)
    
    // If both true, true
    ins += CondMovInstr(EqCond, R8, Immediate(1)) // R8 here should be loc2, but ref compiler used r8
    // If one of it false, false
    ins += CondMovInstr(NeqCond, R8, Immediate(0)) // R8 here should be loc2, but ref compiler used r8

    // Push result
    ins += PushInstr(Seq(R8))
  }

  private def translateOr(
			expr1: Expr, expr2: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    // Expr1 store in R8
    translateExpr(expr1)
    ins += PopInstr(Seq(R8))
    
    // Check if expr1 is true
    ins += CmpInstr(R8, Immediate(1))

    // Allocate new branch name
    val branch_0 = JumpLabel(".L" + branchCounter)
    branchCounter += 1

    // If expr1 true, shortcut to L0
    ins += CondBranchInstr(EqCond, branch_0)

    // Expr2 store in R9
    translateExpr(expr2)
    ins += PopInstr(Seq(R9))

    // Check if expr2 is true
    ins += CmpInstr(R9, Immediate(1))

    // .L0:
    ins += CreateLabel(branch_0)

    
    // If either true, true
    ins += CondMovInstr(EqCond, R8, Immediate(1)) // R8 here should be loc2, but ref compiler used r8
    // If both false, false
    ins += CondMovInstr(NeqCond, R8, Immediate(0)) // R8 here should be loc2, but ref compiler used r8

    // Push result
    ins += PushInstr(Seq(R8))
  }

  private def translateNot(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    // Expr store in R8
    translateExpr(expr)
    ins += PopInstr(Seq(R8))

    // Check expr true or false
		ins += CmpInstr(R8, Immediate(1))
    ins += CondMovInstr(NeqCond, R8, Immediate(1))
    ins += CondMovInstr(EqCond, R8, Immediate(0))

    // Push result
    ins += PushInstr(Seq(R8))
  }

  private def translateNeg(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {

    // Expr store in R8
    translateExpr(expr)
    ins += PopInstr(Seq(R8))

    ins += RsbsInstr(R8, R8, Immediate(0))
    ins += CondBranchLinkInstr(VsCond, CheckOverflow)

    // Push result
    ins += PushInstr(Seq(R8))
  }

  private def translateLen(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    // Expr store in R8
    translateExpr(expr)
    ins += PopInstr(Seq(R8))

    ins += LoadInstr(R8, RegOffset(R8, -4))  // load from a[0] → len a

    // Push result
    ins += PushInstr(Seq(R8))
  }

  private def translateChr(
			expr: Expr
		)(implicit st: SymbolTable, 
               stateST: StateTable,
               ins: ListBuffer[Instruction]) = {
    // Expr store in R8
    translateExpr(expr)
    ins += PopInstr(Seq(R8))

    // Translate to char ASCII
    ins += AndInstr(R8, R8, Immediate(127))

    // Push result
    ins += PushInstr(Seq(R8))
  }

}
