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
    val ptrSize = 4

    val pushFuncRegs = Seq(FP, LR)
    val varNum       = func.symb.findAllVarNum()
    val varRegs      = calculateSaveRegs(varNum)
    val pushedRegNum = pushFuncRegs.size + varRegs.size

    /* Creates new state table that doesn't inherit main's state table */
    implicit val new_stateST = new StateTable(None)
    new_stateST.modifySavedRegs(varRegs)
    new_stateST.modifyVarNum(varNum)

    // First 3 parameters -> R0, R1, R2
    // More parameters -> On stack
    val paraNum = func.params.size
    // Caculate how many stack space needed for more parameters
    new_stateST.updateParamPtr((pushedRegNum + paraNum - (paramReg.size + 1)) * ptrSize)

    // Store function parameters
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
