package wacc

import Ast.Expr
import SymbolObject._

object SemanticChecker {

  var ST: SymbolTable = new SymbolTable(null)

  def checkExpr(expr: Expr): Boolean = {
    true
  }
  
}
