package wacc.CodeGen

import wacc.Ast._
import wacc.Instructions._

import StatTranslator._
import scala.collection.mutable.ListBuffer

object FunctionTranslator {

	def translateFunction(
		func: Func
	)(implicit stateST: StateTable,
						 instrs: ListBuffer[Instruction]): Unit = {

		// Create function label

		// Push register
		

		// Translate function body
		val new_stateST = new StateTable(stateST)
		func.stats.foreach(s => translateStatement(s)(s.symb, new_stateST, instrs))
		
		// Pop register
	
	}
}


