package wacc.CodeGen

import collection.mutable.Map

import wacc.Instructions._

class StateTable(st: StateTable) {
  val dictionary = Map[String, Operand]()
  val encSymTable = st

  /* Add a key-value pair to dictionary */
  def add(name: String, location: Operand) =
    dictionary += (name -> location)

  /* Remove a key-value pair specified by key from dictionary */
  def remove(name: String) =
    dictionary -= name

  /* Look up a value according to key in this symbol table */
  def lookUp(name: String): Option[Operand] =
    dictionary.get(name)

  /* Look up a value according to key in this symbol table and all parent table*/
  def lookUpAll(name: String): Option[Operand] = {
    var s = this
    while (s != null) {
      val obj = s.lookUp(name)
      if (obj != None) {
        return obj
      }
      s = s.encSymTable
    }

    None
  }

}
