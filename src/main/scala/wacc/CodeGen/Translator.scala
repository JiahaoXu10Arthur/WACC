package wacc.CodeGen

import scala.collection.mutable.ListBuffer
import wacc.Instructions._
import wacc.Ast._

import StatTranslator._
import FunctionTranslator._

object Translator {

  def translate(p: Program): List[Instruction] = {
    implicit val stateST = new StateTable(null)
    implicit val instrs = new ListBuffer[Instruction]()

    // Firstly reading the headeres of the functions
    p.funcs.foreach { f => translateFunction(f) }
    // Checking the validity of the statements followed
    p.stats.foreach { s => translateStatement(s)(s.symb, stateST, instrs) }

    instrs.toList
  }
}