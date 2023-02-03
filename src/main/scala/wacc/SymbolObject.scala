package wacc

import Types._

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

		case class ArrayObj(val elemType: Type, val elemNum: Int) extends SymbolObj {
			override def getType(): Type = ArrayType(elemType)
		}

		// case class PairObj(val fstType: Type, 
		// 								   val sndType: Type, 
		// 									 val pairType: Type) extends SymbolObj {
		// 	override def getType(): Type = pairType
		// }

		// case class ArrayLitObj(val elemType: Type) extends SymbolObj {
		// 	override def getType(): Type = elemType
		// }


	}
