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

		val regNum = func.symb.findVarNum()
		// Adding registers to regsForUse
		for (i <- 1 to regNum) {
			regsForUse += regsAvailable(i)
		}
		

		// Create function label
		addInstr(CreateLabel(JumpLabel("wacc_" + func.ident.name)))

		// Push register
		addInstr(PushInstr(funcRegs))
		addInstr(PushInstr(regsForUse.toSeq))

		// Translate function body
		val new_stateST = new StateTable(stateST)
		func.stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir))
		
		// Pop register
	
	}

	
}


