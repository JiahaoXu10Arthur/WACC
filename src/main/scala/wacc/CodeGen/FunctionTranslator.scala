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
    val paraNum = func.params.size

    /* Creates new state table that doesn't inherit main's state table */
    implicit val stateST = new StateTable(None)
    stateST.modifySavedRegs(varRegs)
    stateST.modifyVarNum(varNum)

    /* Caculate how many stack space needed for more parameters */
    //TODO: Refactor this into Utils
    stateST.updateParamPtr((pushedRegNum + paraNum - (paramReg.size + 1)) * ptrSize)

    /* Store function parameters */
    func.params.foreach { param =>
      val loc = stateST.nextParamLocation()
      stateST.addParam(param.ident.name, loc)
    }

    /* Translate function body */   
    addInstr(CreateLabel(WACCFuncLabel(func.ident.name)))
    beginBlock()
    func.stats.foreach { s =>
      translateStatement(s)(s.symb, stateST, ir)
    }

    /* Add the ltorg tag at the end for literal pool refresh */
    addInstr(LtorgTag)
  }

}
