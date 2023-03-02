package wacc.CodeGen

import wacc.Instructions._
import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

import StatTranslator._
import FunctionTranslator._
import IR._
import Utils.{beginBlock, endBlock, calculateSaveRegs}

object Translator {

  private final val DefaultExitCode = 0
  implicit var branchCounter = 0

  def translate(p: Program, mainST: SymbolTable): IR = {
    // Initialize implicit value
    implicit val ir = new IR()
    val varNum = mainST.findAllVarNum()
    val varRegs = calculateSaveRegs(varNum)
    val pushRegs = varRegs ++ reservedReg
    branchCounter = 0

    /* Update state table */
    implicit val stateST = new StateTable(None)
    stateST.modifySavedRegs(pushRegs)
    stateST.modifyVarNum(varNum)

    /* Translates Main body */
    addInstr(CreateLabel(Main))
    beginBlock()
    p.stats.foreach(s => translateStatement(s)(s.symb, stateST, ir))
    addInstr(MovInstr(R0, Immediate(DefaultExitCode)))
    endBlock(restoreSP = false)

    /* Translate functions */
    p.funcs.foreach(f => translateFunction(f))

    /* Return the intermediate representation for code generation */
    returnIR()
  }
}
