package wacc

import collection.mutable.Map
import SymbolObject._
import SymbolObjectType._

class SymbolTable(st: SymbolTable) {

  val dictionary = Map[(String, ObjectType), SymbolObj]()
  val encSymTable = st

  def add(name: String, objType: ObjectType, obj: SymbolObj)
    = dictionary += ((name, objType) -> obj)

  def remove(name: String, objType: ObjectType)
    = dictionary -= ((name, objType))

  def lookUp(name: String, objType: ObjectType): Option[SymbolObj]
    = dictionary.get((name, objType))

  // Lookup name in current and enclosing
  def lookUpAll(name: String, objType: ObjectType): Option[SymbolObj] = {
    var s = this
    while (s != null) {
        val obj = s.lookUp(name, objType)
        if (obj != None) {
          return obj
        }
        s = s.encSymTable
    }

    None
  }

  // Lookup name in current and enclosing
  def lookUpAllWithObjType(name: String, objType: ObjectType, objNeed: SymbolObj): Option[SymbolObj] = {
    var s = this
    while (s != null) {
        val obj = s.lookUp(name, objType)
        if (obj == Some(objNeed)) {
          return obj
        }
        s = s.encSymTable
    }

    None
  }
}
