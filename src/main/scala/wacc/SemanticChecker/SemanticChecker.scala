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
    p.classes.foreach { c => 
      readInClassHeader(c)(overAllSt, semErr)
      checkClassDeclaration(c)(overAllSt, semErr)
    }

    /* Main */
    val mainSt: SymbolTable = new SymbolTable(overAllSt, null)
    // Then reading the headeres of the functions
    p.funcs.foreach(readInFunctionHeader(_)(mainSt, semErr))
    // Checking the validity of function declaration
    p.funcs.foreach(checkFuncDeclare(_)(mainSt, semErr))
    // Checking the validity of the statements followed
    p.stats.foreach(checkStat(_)(mainSt, semErr))
    // Link main symbol table to overall symbol table
    overAllSt.addSubSt(mainSt)

    (semErr.toList, mainSt)
  }
}
