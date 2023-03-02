package wacc.CodeGen

import wacc.Instructions._
import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

import StatTranslator._
import FunctionTranslator._
import IR._
import Utils.{beginBlock, endBlock, calculateSaveRegs}

object Translator {

  implicit var branchCounter = 0

  def translate(p: Program, mainST: SymbolTable): IR = {
    implicit val ir      = new IR()
    branchCounter = 0

    addInstr(CreateLabel(Main))
    val varNum = mainST.findAllVarNum()
    val varRegs = calculateSaveRegs(varNum)
    val pushRegs = varRegs ++ reservedReg

    /* Update state table */
    implicit val stateST = new StateTable(None)
    stateST.modifySavedRegs(pushRegs)
    stateST.modifyVarNum(varNum)

    beginBlock()

    // Translate Main
    p.stats.foreach(s => translateStatement(s)(s.symb, stateST, ir))

    // Mov return code 0
    addInstr(MovInstr(R0, Immediate(0)))

    endBlock(restoreSP = false)

    // Firstly reading the headeres of the functions
    p.funcs.foreach(f => translateFunction(f))

    returnIR()
  }
}
