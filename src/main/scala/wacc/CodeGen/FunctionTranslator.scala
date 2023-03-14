package wacc.CodeGen

import wacc.Ast._
import wacc.Instructions._
import wacc.SemanticChecker.SemanticTypes._

import StatTranslator._
import IRBuilder._
import Utils.{beginBlock, calculateSaveRegs}

object FunctionTranslator {

  def translateFunction(
      func: Func
  )(implicit ir: IRBuilder): Unit = {
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
    stateST.updateParamPtr((pushedRegNum + paraNum - (paramReg.size + 1)) * ptrSize)

    /* Store function parameters */
    func.params.foreach { param =>
      val loc = stateST.nextParamLocation()
      stateST.addParam(param.ident.name, loc)
    }

    // Check function overloading to get correct function label
    val funcName = func.ident.name
    val expectRet  = convertType(func.type1)
    val expectArgs = func.params.map(x => convertType(x.paramType))
    // for function header, search in main st
    val funcLabelName = func.symb.getOverloadFuncName(funcName, expectRet, expectArgs)

    /* Create function label */
    addInstr(CreateLabel(WACCFuncLabel(funcLabelName)))

    /* Translate function body */
    beginBlock()
    addInstr(CreateLabel(WACCFuncBodyLabel(funcLabelName)))
    func.stats.foreach { s =>
      translateStatement(s)(s.symb, stateST, ir)
    }

    /* Add the ltorg tag at the end for literal pool refresh */
    addInstr(LtorgTag)
  }

}
