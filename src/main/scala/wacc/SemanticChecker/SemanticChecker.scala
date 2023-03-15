package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer
import wacc.Ast._
import wacc.Error.Errors._
import StatSemantic._
import FunctionSemantic._
import StructSemantic._

object SemanticChecker {

  def semanticCheck(p: Program): (Seq[WACCError], SymbolTable) = {
    implicit val st: SymbolTable = new SymbolTable(null, null)
    implicit val semErr = new ListBuffer[WACCError]()
    
    // Firstly reading the headeres of the structs
    p.structs.foreach { s => readInStructHeader(s) }
    // Checking the validity of struct declaration
    p.structs.foreach { s => checkStructDeclare(s) }

    // Then reading the headeres of the functions
    p.funcs.foreach { f => readInFunctionHeader(f) }
    // Checking the validity of function declaration
    p.funcs.foreach { f => checkFuncDeclare(f) }
    // Checking the validity of the statements followed
    p.stats.foreach { s => checkStat(s) }

    // (semErr.toList, st.getImmutableTable())
    (semErr.toList, st)
  }
}
