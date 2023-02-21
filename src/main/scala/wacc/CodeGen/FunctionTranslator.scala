package wacc.CodeGen

import wacc.Ast._
import wacc.Instructions._

import StatTranslator._
import scala.collection.mutable.ListBuffer

object FunctionTranslator {

	def translateFunction(
		func: Func
	)(implicit stateST: StateTable,
						 ins: ListBuffer[Instruction]): Unit = {

		val funcRegs = Seq(FP, LR)
		val regsForUse = new ListBuffer[Register]()
		val regsAvailable = Seq(R4, R5, R6, R7)

		val regNum = func.symb.findVarNum()
		// Adding registers to regsForUse
		for (i <- 1 to regNum) {
			regsForUse += regsAvailable(i)
		}
		

		// Create function label
		ins += CreateLabel(JumpLabel("wacc_" + func.ident.name))

		// Push register
		ins += PushInstr(funcRegs)
		ins += PushInstr(regsForUse.toSeq)

		// Translate function body
		val new_stateST = new StateTable(stateST)
		func.stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ins))
		
		// Pop register
	
	}

	
}


