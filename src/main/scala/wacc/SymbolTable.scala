package wacc

import collection.mutable.Map
import SymbolObject._
import SymbolObjectType._
import scala.collection.mutable.ListBuffer

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

  // Find similar SymbolObject with the given name to provide as suggestion
  def lookUpSimilar(typeIn: String, objType: ObjectType): Seq[(String, (Int, Int))] = {
    // val similar: Seq[(String, (Int, Int))] = Seq()
    val similar = new ListBuffer[(String, (Int, Int))]()

    dictionary.foreach{x => {
      if (x._1._1.toUpperCase() == typeIn.toUpperCase())
        /* x._1._1 -> name */
        similar += ((x._1._1, x._2.getPos()))
    }}

    return similar.toSeq
  }

  // Find similar SymbolObject with the given name to provide as suggestion in all scope
  def lookUpAllSimilar(typeIn: String, objType: ObjectType): Seq[(String, (Int, Int))] = {
    // val similar: Seq[(String, (Int, Int))] = Seq()
    val similar = new ListBuffer[(String, (Int, Int))]()

    var s = this
    while (s != null) {
        val new_similar: Seq[(String, (Int, Int))] = s.lookUpSimilar(typeIn, objType)
        similar ++= new_similar
        s = s.encSymTable
    }

    return similar.toSeq
  }
}
