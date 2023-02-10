package wacc

import SymbolObject._
import SymbolObjectType._

class ImmutableSymbolTable(final val dictionary: Map[(String, ObjectType), SymbolObj],
                           final val subDics: List[SymbolTable]) {
}