package wacc.CodeGen

import wacc.Instructions._
import wacc.CodeGen.IR._
import wacc.CodeGen.StateTable

object Utils {
  def endBlock()(implicit stateT: StateTable, ir: IR): Unit = {
    val popFuncRegs    = Seq(FP, PC)
    val savedRegs      = stateT.getSavedRegs()
    val stackSpace     = (savedRegs.size - variableReg.size) * 4

    addInstr(MovInstr(SP, FP))
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
