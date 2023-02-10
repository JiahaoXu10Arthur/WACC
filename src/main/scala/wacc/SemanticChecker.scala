package wacc

import Ast._
import StatSemantic._
import FunctionSemantic._
import scala.collection.mutable.ListBuffer
import Errors._

object SemanticChecker {

  def semanticCheck(p: Program): (Seq[WACCError], ImmutableSymbolTable) = {
    implicit val st: SymbolTable = new SymbolTable(null)
    implicit val semErr = new ListBuffer[WACCError]()
    
    // Firstly reading the headeres of the functions
    p.funcs.foreach { f => readInFunctionHeader(f) }
    // Checking the validity of function declaration
    p.funcs.foreach { f => checkFuncDeclare(f) }
    // Checking the validity of the statements followed
    p.stats.foreach { s => checkStat(s) }

    (semErr.toList, st.getImmutableTable())
  }
}
