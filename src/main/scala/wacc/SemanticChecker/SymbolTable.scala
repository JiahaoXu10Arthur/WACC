package wacc.SemanticChecker

import collection.mutable.Map
import scala.collection.mutable.ListBuffer

import SymbolObject._
import SymbolObjectType._

class SymbolTable(st: SymbolTable) {

  val dictionary = Map[(String, ObjectType), SymbolObj]()
  val encSymTable = st
  val subSts = ListBuffer[SymbolTable]()

  /* Add sub symbol table to child list */
  def addSubSt(subSt: SymbolTable) = {
    subSts += subSt
  }

  /* Add a key-value pair to dictionary */
  def add(name: String, objType: ObjectType, obj: SymbolObj) =
    dictionary += ((name, objType) -> obj)

  /* Remove a key-value pair specified by key from dictionary */
  def remove(name: String, objType: ObjectType) =
    dictionary -= ((name, objType))

  /* Look up a value according to key in this symbol table */
  def lookUp(name: String, objType: ObjectType): Option[SymbolObj] =
    dictionary.get((name, objType))

  /* Look up a value according to key in this symbol table and all parent table*/
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

  /* Find similar value in this st with the given key provided as suggestion
     Similar means: without case sensitivity */
  def lookUpSimilar(
      typeIn: String,
      objType: ObjectType
  ): Set[(String, (Int, Int))] = {
    val similar = new ListBuffer[(String, (Int, Int))]()

    /* If names are the same without case sensitive, they are similar */
    dictionary.foreach { x =>
      {
        if (x._1._1.toUpperCase() == typeIn.toUpperCase())
          similar += ((x._1._1, x._2.getPos()))
      }
    }

    return similar.toSet
  }

  /* Find similar value in this st and all parent st,
     with the given key provided as suggestion
     Similar means: without case sensitivity */
  def lookUpAllSimilar(
      typeIn: String,
      objType: ObjectType
  ): Set[(String, (Int, Int))] = {
    val similar = new ListBuffer[(String, (Int, Int))]()

    /* Recursively find similar set until the top parent st */
    var s = this
    while (s != null) {
      val new_similar: Set[(String, (Int, Int))] =
        s.lookUpSimilar(typeIn, objType)
      /* Concatenate all similar found */
      similar ++= new_similar
      s = s.encSymTable
    }

    return similar.toSet
  }

  /* Get an immutable symbol table based on the current one */
  def getImmutableTable(): ImmutableSymbolTable = {
    return new ImmutableSymbolTable(dictionary.toMap, subSts.toList)
  }
}
