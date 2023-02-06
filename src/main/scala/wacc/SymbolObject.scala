package wacc

import SemanticType._

object SymbolObject {

	sealed trait SymbolObj {
		def getType(): Type
	}

		case class VariableObj(val t: Type) extends SymbolObj {
			override def getType(): Type = t
		}

		case class FuncObj(val returnType: Type, 
											 val args: List[VariableObj], 
											 val argc: Int,
											 val symTable: SymbolTable) extends SymbolObj {
			override def getType(): Type = returnType
		}
	}
