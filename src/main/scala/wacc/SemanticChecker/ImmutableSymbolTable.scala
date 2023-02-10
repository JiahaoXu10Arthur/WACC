package wacc.SemanticChecker

import SymbolObject._
import SymbolObjectType._

class ImmutableSymbolTable(val dictionary: Map[(String, ObjectType), SymbolObj],
                           val subDics: List[SymbolTable]) {
}