package wacc.CodeGen

import wacc.Ast._
import wacc.Instructions._
import wacc.CodeGen.IRBuilder._
import wacc.CodeGen.StateTable
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes._

import ExprTranslator.{translateExpr}

object Utils {
  def calculateSaveRegs(varNum: Int): Seq[Register] = {
    if (varNum <= variableReg.size) {
      (0 until varNum).map(i => variableReg(i))
    }
    else {
      variableReg
    }
  }

  def beginBlock()(implicit stateT: StateTable, ir: IRBuilder): Unit = {
    val pushFuncRegs = Seq(FP, LR)
    val varRegs      = stateT.getSavedRegs()
    val varNum       = stateT.getVarNum()

    // Push register
    addInstr(PushInstr(pushFuncRegs))
    if (!varRegs.isEmpty) {
      addInstr(PushInstr(varRegs))
    }
    
    addInstr(MovInstr(FP, SP))

    // variable stack space
    val stackSpace = (varNum - variableReg.size) * 4
    // Add stack space if too many variables
    if (stackSpace > 0) {
      addInstr(SubInstr(SP, SP, Immediate(stackSpace)))
      // Update stateTable fp pointer
      stateT.updateFPPtr(stackSpace * -1)
    }

  }

  def endBlock(restoreSP: Boolean)(implicit stateT: StateTable, ir: IRBuilder): Unit = {
    val popFuncRegs    = Seq(FP, PC)
    val savedRegs      = stateT.getSavedRegs()
    val regNum         = stateT.getVarNum()
    val stackSpace     = (regNum - variableReg.size) * 4

    if (restoreSP) {
      // Restore SP
      addInstr(MovInstr(SP, FP))
    }
    
    // Add stack space if too many variables
    if (stackSpace > 0) {
      addInstr(AddInstr(SP, SP, Immediate(stackSpace)))
    }
    // Pop Register on stack
		if (!savedRegs.isEmpty){
			addInstr(PopInstr(savedRegs))
		}
    
    addInstr(PopInstr(popFuncRegs))
  }

  /* Translate Expr and pop result to given register */
  def translateExprTo(expr: Expr, 
                      reg: Register)(implicit st: SymbolTable, 
                                              stateST: StateTable, 
                                              ir: IRBuilder) = {
    translateExpr(expr)
    addInstr(PopInstr(Seq(reg)))                              
  }

  /* Translate two Expr and pop them to corresponding register */
  def translateTwoExprTo(expr1: Expr, 
                         expr2: Expr,
                         reg1: Register,
                         reg2: Register)(implicit st: SymbolTable, 
                                              stateST: StateTable, 
                                              ir: IRBuilder) = {
    translateExpr(expr1)
    translateExpr(expr2)
    addInstr(PopInstr(Seq(reg2)))
    addInstr(PopInstr(Seq(reg1)))                                
  }

  /* Find variable by name in stateTable to find its location */
  def findVarLoc(identifier: String, stateST: StateTable): Location =
    stateST.lookUpAll(identifier) match {
      case Some(location) => location
      case _              => null
    }

  /* Find variable by name in stateTable to find its location */
  def findLvalueLoc(lvalue: Lvalue, stateST: StateTable): Location =
    lvalue match {
      case Ident(name)             => findVarLoc(name, stateST)
      case ArrayElem(ident, index) => findVarLoc(ident.name, stateST)
      case PairElem(index, lvalue) => findLvalueLoc(lvalue, stateST)
    }

  def sizeOfElem(elemType: Type): Int =
    elemType match {
      case BoolType() => 1
      case CharType() => 1
      case _          => 4
    }

  def callerSavePush()(implicit stateST: StateTable, ir: IRBuilder) = {
    val usedParam = stateST.getUsedParamRegs()
    addInstr(Comment(s"Pushing param registers here! Number: ${usedParam.size}"))
    if (!usedParam.isEmpty) {
      addInstr(PushInstr(usedParam))
    }
  }

  def callerSavePop()(implicit stateST: StateTable, ir: IRBuilder) = {
    val usedParam = stateST.getUsedParamRegs()
    addInstr(Comment(s"Poping param registers here! Number: ${usedParam.size}"))
    if (!usedParam.isEmpty) {
      addInstr(PopInstr(usedParam))
    }
  }
}
