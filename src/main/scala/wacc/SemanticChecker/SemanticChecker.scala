package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer
import wacc.Ast._
import wacc.Error.Errors._
import StatSemantic._
import FunctionSemantic._
import StructSemantic._
import ClassSemantic._

object SemanticChecker {

  def semanticCheck(p: Program): (Seq[WACCError], SymbolTable) = {
    val overAllSt: SymbolTable = new SymbolTable(null, null)
    val semErr = new ListBuffer[WACCError]()
    
    /* Struct */
    // Firstly reading the headeres of the structs
    p.structs.foreach { s => readInStructHeader(s)(overAllSt, semErr) }
    // Checking the validity of struct declaration
    p.structs.foreach { s => checkStructDeclare(s)(overAllSt, semErr) }

    /* Class */
    // Reading the headeres of the classes
    // Every class will decalre a new class symbol table
    p.classes.foreach { c => readInClassHeader(c)(overAllSt, semErr) }
    // Checking the validity of class declaration
    p.classes.foreach { c => checkClassDeclaration(c)(overAllSt, semErr)}

    /* Main */
    val mainSt: SymbolTable = new SymbolTable(overAllSt, null)
    // Reading the headeres of main functions
    p.funcs.foreach(f => readInFunctionHeader(f)(mainSt, semErr))
    // Checking the validity of function declaration
    p.funcs.foreach(f => checkFuncDeclare(f)(mainSt, semErr))
    // Checking the validity of the statements followed
    p.stats.foreach(s => checkStat(s)(mainSt, semErr))
    
    // Link main symbol table to overall symbol table
    overAllSt.addSubSt(mainSt)

    (semErr.toList, mainSt)
  }
}
