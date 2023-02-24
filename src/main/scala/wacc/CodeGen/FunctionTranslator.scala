package wacc.CodeGen

import scala.collection.mutable

import wacc.Ast._
import wacc.Instructions._

import StatTranslator._
import IR._

object FunctionTranslator {

	def translateFunction(
		func: Func
	)(implicit stateST: StateTable,
			   ir: IR): Unit = {

		val funcRegs = Seq(FP, LR)
		val regsForUse = new mutable.ListBuffer[Register]()
		val regsAvailable = Seq(R4, R5, R6, R7)
		var flag = false

		val regNum = func.symb.findVarNum()
		if (regNum <= 4) {
			// Adding registers to regsForUse if the registers are enough
			for (i <- 1 to regNum) {
				regsForUse += regsAvailable(i)
			}
		} else {
			// Adding the first four variables to registers and push others to stack
			regsForUse ++= regsAvailable
			flag = true
		}

		// Push register
		addInstr(PushInstr(funcRegs))
		addInstr(PushInstr(regsForUse.toSeq))
		addInstr(MovInstr(FP, SP))
		
		// Add stack space if too many variables
		if (flag) {
			val stackSpace = (regNum - 4) * 4
			addInstr(SubInstr(SP, SP, Immediate(stackSpace)))
		}
		
		
		// Create function label
		addInstr(CreateLabel(JumpLabel("wacc_" + func.ident.name)))

		// Translate function body
		val new_stateST = new StateTable(Some(stateST))
		func.stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir))
		
		// Pop register
		addInstr(PopInstr(regsForUse.toSeq))
		addInstr(PopInstr(funcRegs))
	
	}

	
}


