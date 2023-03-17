package wacc.SemanticChecker

object SymbolObjectType {
  sealed trait ObjectType
    case class VariableType()  extends ObjectType
    case class FunctionType()  extends ObjectType
    case class StructObjType() extends ObjectType
    case class ClassObjType()  extends ObjectType
}