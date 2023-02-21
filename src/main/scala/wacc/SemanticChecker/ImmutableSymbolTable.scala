package wacc.SemanticChecker

import SymbolObject._
import SymbolObjectType._

class ImmutableSymbolTable(
	final val dictionary: Map[(String, ObjectType), SymbolObj],
	final val subDics: List[SymbolTable]) {
	
	// Used for main and func translation
	def findVarNum(): Int = {
		var var_num = 0
		
		for (key_value <- dictionary) {
			key_value._1 match {
				case (_, VariableType()) => var_num += 1
				case _ =>
			}
		}

		var_num
	}

}

