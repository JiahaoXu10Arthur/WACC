package wacc.CodeGen

import wacc.Ast._
import wacc.Instructions._
import wacc.CodeGen.IR._
import wacc.CodeGen.StateTable
import wacc.SemanticChecker.SemanticTypes._

object Utils {
  def calculateSaveRegs(varNum: Int): Seq[Register] = {
    var saveRegs: Seq[Register] = Seq()
    if (varNum <= variableReg.size) {
      saveRegs = (0 until varNum).map(i => variableReg(i))
    }
    else {
      saveRegs = variableReg
    }
    saveRegs
  }

  def beginBlock()(implicit stateT: StateTable, ir: IR): Unit = {
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

  def endBlock(restoreSP: Boolean)(implicit stateT: StateTable, ir: IR): Unit = {
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

  def callerSavePush()(implicit stateST: StateTable, ir: IR) = {
    val usedParam = stateST.getUsedParamRegs()
    addInstr(Comment(s"Pushing param registers here! Number: ${usedParam.size}"))
    if (!usedParam.isEmpty) {
      addInstr(PushInstr(usedParam))
    }
  }

  def callerSavePop()(implicit stateST: StateTable, ir: IR) = {
    val usedParam = stateST.getUsedParamRegs()
    addInstr(Comment(s"Poping param registers here! Number: ${usedParam.size}"))
    if (!usedParam.isEmpty) {
      addInstr(PopInstr(usedParam))
    }
  }
}
