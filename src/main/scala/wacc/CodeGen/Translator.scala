package wacc.CodeGen

import wacc.Instructions._
import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

import StatTranslator._
import FunctionTranslator._
import IRBuilder._
import TailRecOptimiser._
import Utils.{ beginBlock, calculateSaveRegs, endBlock }

object Translator {
  implicit var branchCounter = 0

  def translate(p: Program, mainST: SymbolTable, tailRecOpt: Boolean): IR = {

    // Translate Main

    // Initialize implicit value
    implicit val ir = new IRBuilder()
    val varNum      = mainST.findAllVarNum()
    val varRegs     = calculateSaveRegs(varNum)
    val pushRegs    = varRegs ++ reservedReg
    branchCounter = 0

    /* Update state table */
    implicit val stateST = new StateTable(None)
    stateST.modifySavedRegs(pushRegs)
    stateST.modifyVarNum(varNum)

    /* Translates Main body */
    addInstr(GlobalTag)
    addInstr(CreateLabel(Main))
    beginBlock()
    p.stats.foreach(s => translateStatement(s)(s.symb, stateST, ir))
    addInstr(MovInstr(R0, DefaultExitCodeImm))
    endBlock(restoreSP = false)

    /* Translate Class function */
    p.classes.foreach(c => c.funcs.foreach(
                      f => translateFunction(c.struct.name.name, f)(c.symb, ir)))

    /* Translate functions */
    p.funcs.foreach(f =>
      translateFunction("main", tailRecOpt match {
        case true  => optimiseFunc(f)
        case false => f
      })(f.symb, ir)
    )

    /* Return the intermediate representation for code generation */
    returnIR()
  }
}
