package wacc

object SymbolObjectType {
  sealed trait ObjectType
		case class VariableType() extends ObjectType
		case class FunctionType() extends ObjectType
}
