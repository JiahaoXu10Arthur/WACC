package wacc

import collection.mutable.Map
import SymbolObject._

class SymbolTable(st: SymbolTable) {

  val dictionary = Map[String, SymbolObj]()
  val encSymTable = st

  def add(name: String, obj: SymbolObj)
    = dictionary += (name -> obj)

  def lookUp(name: String): Option[SymbolObj]
    = dictionary.get(name)

  // Lookup name in current and enclosing
  def lookUpAll(name: String): Option[SymbolObj] = {
    var s = this
    while (s != null) {
        val obj = s.lookUp(name)
        s = s.encSymTable
        if (obj != None) {
            obj
        }
    }

    None
  }
}
