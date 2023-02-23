package wacc.CodeGen

import scala.collection.mutable.ListBuffer
import wacc.Instructions._
import wacc.Ast._
import wacc.SemanticChecker.ImmutableSymbolTable

import StatTranslator._
import FunctionTranslator._
import IR._


object Translator {

  def translate(p: Program, mainST: ImmutableSymbolTable): IR = {

    implicit val stateST = new StateTable(null)
    implicit val ir = new IR()

    addInstr(CreateLabel(JumpLabel("main:")))

		val regsForUse = new ListBuffer[Register]()
		val regsAvailable = Seq(R4, R5, R6, R7)
		val reservedRegs = Seq(R8, R10, R12)

		val regNum = mainST.findVarNum()
		// Adding registers to regsForUse
		for (i <- 1 to regNum) {
			regsForUse += regsAvailable(i)
		}
		
		val pushRegs = regsForUse.toSeq ++ reservedRegs

    // Push register
    addInstr(PushInstr(Seq(FP, LR)))
		addInstr(PushInstr(pushRegs))

    // Translate Main
    p.stats.foreach { s => translateStatement(s)(s.symb, stateST, ir) }

    // Mov return code 0
    addInstr(MovInstr(R0, Immediate(0)))

    // Pop register
    addInstr(PopInstr(pushRegs))
    addInstr(PopInstr(Seq(FP, PC)))

    // Firstly reading the headeres of the functions
    p.funcs.foreach { f => translateFunction(f) }

    returnIR()
  }
}