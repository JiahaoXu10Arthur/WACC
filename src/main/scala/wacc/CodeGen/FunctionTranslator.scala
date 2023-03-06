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

    /* Translate function body */
    // Check function overloading to get correct function label
    val funcName = func.ident.name
    val funcs = func.symb.lookUpAllFunc(funcName).get
    var funcLabelName = funcName
    // if overloading
    if (funcs.length > 1) {
      val argTypes = func.params.map(x => convertType(x.paramType))
      val funcOverloadIndex = func.symb.getOverloadFuncIndex(funcName, argTypes)
      funcLabelName = funcName + funcOverloadIndex
    } 
    addInstr(CreateLabel(WACCFuncLabel(funcLabelName)))

    beginBlock()
    func.stats.foreach { s =>
      translateStatement(s)(s.symb, stateST, ir)
    }

    /* Add the ltorg tag at the end for literal pool refresh */
    addInstr(LtorgTag)
  }

}
