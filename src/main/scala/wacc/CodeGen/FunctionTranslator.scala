package wacc.CodeGen

import scala.collection.mutable

import wacc.Ast._
import wacc.Instructions._

import StatTranslator._
import IR._

object FunctionTranslator {

	def translateFunction(
		func: Func
	)(implicit ir: IR): Unit = {

    val pushFuncRegs   = Seq(FP, LR)
    val regsForUse = mutable.ListBuffer[Register]()

    // Find overall viriable number in the function
    val regNum = func.symb.findAllVarNum()

    if (regNum <= 4) {
      // Adding registers to regsForUse if the registers are enough
      for (i <- 0 until regNum)
        regsForUse += variableReg(i)
    } else {
      // Adding the first four variables to registers and push others to stack
      regsForUse ++= variableReg
    }

		// Create function label
		addInstr(CreateLabel(WACCFuncLabel(func.ident.name)))

		// Push register
		addInstr(PushInstr(pushFuncRegs))
		if (!regsForUse.isEmpty){
			addInstr(PushInstr(regsForUse.toSeq))
		}
		addInstr(MovInstr(FP, SP))

    var pushedRegNum = pushFuncRegs.size
    pushedRegNum += regsForUse.size

		// Function does not inherit main's state table
		val new_stateST = new StateTable(None)
    new_stateST.modifySavedRegs(regsForUse.toSeq)
    new_stateST.modifyPushedRegNum(regNum)

    // variable stack space
    val stackSpace = (regNum - variableReg.size) * 4
    // Add stack space if too many variables
    if (stackSpace > 0) {
      addInstr(SubInstr(SP, SP, Immediate(stackSpace)))
      // Update stateTable fp pointer
      new_stateST.updateFPPtr(stackSpace * (-1))
    }

    // Store parameter
    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val para_len = func.params.size

    new_stateST.updateParamPtr((pushedRegNum + para_len - 4) * 4)

    func.params.foreach{param => 
      val loc = new_stateST.nextParamLocation()
      new_stateST.addParam(param.ident.name, loc)
    }
		
    val paramRegs = new_stateST.getUsedParamRegs()
    addInstr(Comment(s"Parameter nums ${paramRegs.size}"))

		// Translate function body
		func.stats.foreach(s => {
      translateStatement(s)(s.symb, new_stateST, ir) 
      addInstr(Comment(s"Statement translated, var num ${new_stateST.getUsedRegs().size}"))
    })

    // Add the ltorg tag at the end
    addInstr(Tag(".ltorg"))
  }


}
