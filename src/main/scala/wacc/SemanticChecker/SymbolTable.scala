package wacc.SemanticChecker

import scala.collection.mutable

import SymbolObject._
import SymbolObjectType._

class SymbolTable(st: SymbolTable, tableType: SymbolObjectType.ObjectType) {

  val dictionary = mutable.Map[(String, ObjectType), SymbolObj]()
  val subSts = mutable.ListBuffer[SymbolTable]()
  val encSymTable = st
  // SymbolTable has 2 type - 
  // Variable type -> new scope within same function
  // Function type -> new function scope
  val stType = tableType

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

  // find variable number in this scope
  private def findVarNum(): Int = {
    var var_num = 0
    
    for (key_value <- dictionary) {
      key_value._2 match {
        case VariableObj(_, _) => var_num += 1
        case _ =>
      }
    }

    var_num
	}

	private def findAllVarNumHelper(st: SymbolTable) : Int = {
		var num = st.findVarNum()

		st.subSts.foreach{subST => 
			subST.stType match {
				case VariableType() => {
					num += findAllVarNumHelper(subST)
				}
				case _ =>
		}}

		num
	}
		
	// find variable number in all scope within function
  def findAllVarNum(): Int = {
    findAllVarNumHelper(this)
  }

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
    val similar = new mutable.ListBuffer[(String, (Int, Int))]()

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
    val similar = new mutable.ListBuffer[(String, (Int, Int))]()

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
  def getImmutableTable(): (Map[(String, ObjectType), SymbolObj], 
                            List[SymbolTable]) = {
    (dictionary.toMap, subSts.toList)
  }
}
