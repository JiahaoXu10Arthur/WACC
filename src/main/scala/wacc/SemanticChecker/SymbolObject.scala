package wacc.SemanticChecker

import SemanticTypes._
import wacc.Ast._

object SymbolObject {

  sealed trait SymbolObj {
    def getType(): Type
    def getPos(): (Int, Int)
  }

  case class VariableObj(val t: Type, val pos: (Int, Int)) extends SymbolObj {
    override def getType(): Type = t
    override def getPos(): (Int, Int) = pos
  }

  case class ParamObj(val t: Type, val pos: (Int, Int)) extends SymbolObj {
    override def getType(): Type = t
    override def getPos(): (Int, Int) = pos
  }

  case class FuncObj(
      val returnType: Type,
      val args: List[ParamObj],
      val argc: Int,
      val symTable: SymbolTable,
      val funcPos: (Int, Int)
  ) extends SymbolObj {
    override def getType(): Type = returnType
    override def getPos(): (Int, Int) = funcPos
  }

  case class StructObj(
      val ident: Ident,
      val fields: List[(Ident, VariableObj)],
      val symTable: SymbolTable,
      val structPos: (Int, Int)
  ) extends SymbolObj {
    override def getType(): Type = StructType(ident)
    override def getPos(): (Int, Int) = structPos
  }
}
