package wacc.CodeGen

import scala.collection.mutable.ListBuffer
import wacc.Instructions._
import wacc.Ast._
import wacc.SemanticChecker.ImmutableSymbolTable

import StatTranslator._
import FunctionTranslator._


object Translator {

  def translate(p: Program, mainST: ImmutableSymbolTable): List[Instruction] = {

    implicit val stateST = new StateTable(null)
    implicit val ins = new ListBuffer[Instruction]()

    ins += CreateLabel(JumpLabel("main:"))

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
		ins += PushInstr(Seq(FP, LR))
		ins += PushInstr(pushRegs)

    // Translate Main
    p.stats.foreach { s => translateStatement(s)(s.symb, stateST, ins) }

    // Mov return code 0
    ins += MovInstr(R0, Immediate(0))

    // Pop register
    ins += PopInstr(pushRegs)
    ins += PopInstr(Seq(FP, PC))

    // Firstly reading the headeres of the functions
    p.funcs.foreach { f => translateFunction(f) }

    ins.toList
  }
}