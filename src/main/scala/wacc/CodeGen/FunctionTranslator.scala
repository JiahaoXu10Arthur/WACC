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
			   		 ir: IR,
						 branchCounter: Int): Unit = {

		val funcRegs = Seq(FP, LR)
		val regsForUse = mutable.ListBuffer[Register]()
		var stackInUse = false

		// Find overall viriable number in the function
		val regNum = func.symb.findAllVarNum()

		if (regNum <= 4) {
			// Adding registers to regsForUse if the registers are enough
			for (i <- 0 until regNum) {
				regsForUse += variableReg(i)
			}
		} else {
			// Adding the first four variables to registers and push others to stack
			regsForUse ++= variableReg
			stackInUse = true
		}

		// Push register
		addInstr(PushInstr(funcRegs))
		addInstr(PushInstr(regsForUse.toSeq))
		addInstr(MovInstr(FP, SP))
		
		val stackSpace = (regNum - 4) * 4
		// Add stack space if too many variables
		if (stackInUse) {
			addInstr(SubInstr(SP, SP, Immediate(stackSpace)))
		}

		// Add parameter to state table

		// Store parameter
    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val para_len = func.params.size
    var index = 0
		
		while (index < para_len) {
      // First 3 parameters
      if (index < 3) {

        // Change it later, should have pool of usable register
        val reg = 
          index match {
            case 0 => R0
            case 1 => R1
            case 2 => R2
          }
				stateST.add(func.params(index).ident.name, reg)

			} else {

				// may need to check, does not need to specify where it is?
				stateST.add(func.params(index).ident.name, RegIntOffset(SP, -4))
			}

			index += 1
    }
		
		// Create function label
		addInstr(CreateLabel(JumpLabel("wacc_" + func.ident.name)))

		// Translate function body
		val new_stateST = new StateTable(Some(stateST))
		// Update stateTable fp pointer
		new_stateST.updateFPPtr(stackSpace * -1)

		func.stats.foreach(s => translateStatement(s)(s.symb, new_stateST, ir, branchCounter))
		
		// Pop register
		addInstr(PopInstr(regsForUse.toSeq))
		addInstr(PopInstr(funcRegs))
	
	}

	
}


