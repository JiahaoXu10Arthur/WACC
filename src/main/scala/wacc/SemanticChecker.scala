package wacc

import Ast._
import StatSemantic._

object SemanticChecker {

  var st: SymbolTable = new SymbolTable(null)

  def semanticCheck(p: Program): Unit = {
    
  }

  def semanticErr(where: String) = {
    throw new SemanticErr("Semantic Error in " + where)
  }

  case class SemanticErr(private val message: String = "Semantic Error", 
                         private val cause: Throwable = None.orNull)
                         extends Exception(message, cause) 

}
