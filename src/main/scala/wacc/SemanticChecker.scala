package wacc

import Ast._
import StatSemantic._
import FunctionSemantic._
import scala.collection.mutable.ListBuffer
import Errors._

object SemanticChecker {

  def semanticCheck(p: Program): Seq[WACCError] = {
    implicit val st: SymbolTable = new SymbolTable(null)
    implicit val semErr = new ListBuffer[WACCError]()

    p.funcs.foreach { f => readInFunctionHeader(f) }
    p.funcs.foreach { f => checkFuncDeclare(f) }
    p.stats.foreach { s => checkStat(s) }

    semErr.toList
  }
}
