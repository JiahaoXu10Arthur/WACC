package wacc.CodeGen

import scala.collection.mutable.ListBuffer
import wacc.Instructions._
import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

import StatTranslator._
import FunctionTranslator._
import IR._

object Translator {

	implicit var branchCounter = 0

  def translate(p: Program, mainST: SymbolTable): IR = {

		implicit val stateST = new StateTable(None)
  	implicit val ir = new IR()
		branchCounter = 0
    
    addInstr(CreateLabel(Main))

		val regsForUse = new ListBuffer[Register]()
    var stackInUse = false

		val varNum = mainST.findAllVarNum()
    
		// Adding registers to regsForUse
		if (varNum <= 4) {
			// Adding registers to regsForUse if the registers are enough
			for (i <- 0 until varNum) {
				regsForUse += variableReg(i)
			}
		} else {
			// Adding the first four variables to registers and push others to stack
			regsForUse ++= variableReg
			stackInUse = true
		}
		
		val pushRegs = regsForUse.toSeq ++ reservedReg

    // Push register
    addInstr(PushInstr(Seq(FP, LR)))
		addInstr(PushInstr(pushRegs))
		addInstr(MovInstr(FP, SP))

		val stackSpace = (varNum - 4) * 4
    // Add stack space if too many variables
		if (stackInUse) {
			addInstr(SubInstr(SP, SP, Immediate(stackSpace)))
		}

    // Translate Main
    p.stats.foreach { s => translateStatement(s)(s.symb, stateST, ir) }

    // Mov return code 0
    addInstr(MovInstr(R0, Immediate(0)))

    // Pop register
    addInstr(PopInstr(pushRegs))
    addInstr(PopInstr(Seq(FP, PC)))

		// Update stateTable fp pointer
		stateST.updateFPPtr(stackSpace * -1)
    // Firstly reading the headeres of the functions
    p.funcs.foreach { f => translateFunction(f) }

    returnIR()
  }
}