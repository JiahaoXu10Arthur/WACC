package wacc

import SemanticType._

object SymbolObject {

	sealed trait SymbolObj {
		def getType(): Type
	}

		case class VariableObj(val t: Type) extends SymbolObj {
			override def getType(): Type = t
		}

		case class ParamObj(val t: Type) extends SymbolObj {
			override def getType(): Type = t
		}

		case class FuncObj(val returnType: Type, 
											 val args: List[ParamObj], 
											 val argc: Int,
											 val symTable: SymbolTable) extends SymbolObj {
			override def getType(): Type = returnType
		}
	}
