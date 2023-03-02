package wacc.CodeGen

import wacc.Ast._
import wacc.Instructions._

import StatTranslator._
import IR._
import Utils.{beginBlock, calculateSaveRegs}

object FunctionTranslator {

  def translateFunction(
      func: Func
  )(implicit ir: IR): Unit = {
    val pushFuncRegs = Seq(FP, LR)
    val varNum       = func.symb.findAllVarNum()
    val varRegs      = calculateSaveRegs(varNum)
    val pushedRegNum = pushFuncRegs.size + varRegs.size

    /* Creates new state table that doesn't inherit main's state table */
    implicit val new_stateST = new StateTable(None)
    new_stateST.modifySavedRegs(varRegs)
    new_stateST.modifyVarNum(varNum)

    // Store parameter
    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val para_len = func.params.size
    new_stateST.updateParamPtr((pushedRegNum + para_len - 4) * 4)
    func.params.foreach { param =>
      val loc = new_stateST.nextParamLocation()
      new_stateST.addParam(param.ident.name, loc)
    }

    /* Add function label */
    addInstr(CreateLabel(WACCFuncLabel(func.ident.name)))

    beginBlock()

    /* Translate function body */
    func.stats.foreach { s =>
      translateStatement(s)(s.symb, new_stateST, ir)
    }

    // Add the ltorg tag at the end
    addInstr(LtorgTag)
  }

}
