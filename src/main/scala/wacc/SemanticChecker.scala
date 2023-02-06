package wacc

import Ast._
import StatSemantic._
import FunctionSemantic._

object SemanticChecker {

  def semanticCheck(p: Program): Unit = {
    val st: SymbolTable = new SymbolTable(null)

    p.funcs.foreach{f => readInFunctionHeader(f, st)}
    p.funcs.foreach{f => checkFuncDeclare(f, st)}
    p.stats.foreach{s => {
      s match {
        case Return(_) => semanticErr("Main cannot return")
        case _ => checkStat(s, st)
      }
    }
   }
  }

  def semanticErr(where: String) = {
    throw new SemanticErr("Semantic Error in " + where)
  }

  case class SemanticErr(private val message: String = "Semantic Error", 
                         private val cause: Throwable = None.orNull)
                         extends Exception(message, cause) 

}
