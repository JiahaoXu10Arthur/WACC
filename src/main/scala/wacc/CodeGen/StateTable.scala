package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._

class StateTable(st: Option[StateTable]) {

  val encStateTable = st
  val dictionary    = mutable.Map[String, Location]()
  val usedReg: mutable.ListBuffer[Register] =
    encStateTable match {
      case Some(upSt) => mutable.ListBuffer[Register]() ++= upSt.getUsedRegs()
      case None => mutable.ListBuffer[Register]()
    }

  var fpPtr: Int =
    encStateTable match {
      case Some(upSt) => upSt.fpPtr
      // First state table
      case None => 0
    }

  /* Add a key-value pair to dictionary */
  def add(name: String, location: Location) = {
    dictionary += (name -> location)

    // add used register
    location match {
      case loc: Register => usedReg += loc
      case _             => updateFPPtr(nextFPPtr())
    }
  }

  /* Remove a key-value pair specified by key from dictionary */
  def remove(name: String) = {
    val location = lookUpAll(name)

    // delete used register
    location match {
      case Some(loc) =>
        loc match {
          case loc: Register => usedReg -= loc
          case _             =>
        }
      case _ =>
    }

    dictionary -= name
  }

  /* Look up a value according to key in this symbol table */
  def lookUp(name: String): Option[Location] =
    dictionary.get(name)

  /* Recursive look up all */
  def lookUpAllHelper(name: String, st: Option[StateTable]): Option[Location] =
    // Check stateTable not null
    st match {
      case Some(stateTable) =>
        // lookUp in this stateTable
        val returnLoc = stateTable.lookUp(name)
        returnLoc match {
          // If found, return
          case Some(loc) => returnLoc
          // If not found, recursive find
          case None => lookUpAllHelper(name, stateTable.encStateTable)
        }
      // if cannot find in all stateTable, None
      case None => None
    }

  /* Look up a value according to key in this symbol table and all parent table*/
  def lookUpAll(name: String): Option[Location] =
    lookUpAllHelper(name, Some(this))

  def updateFPPtr(num: Int) =
    fpPtr = num

  private def nextFPPtr() = fpPtr + 4

  /* Return the next location for variable storage */
  def nextStoreLocation(): Location = {

    // Find available register first
    val unUsedReg = variableReg.filterNot(usedReg.toSet)

    // If has unused reg
    if (!unUsedReg.isEmpty) {
      unUsedReg.head
    } else {
      // Go to storage
      val returnLoc = RegIntOffset(FP, fpPtr)
      returnLoc
    }

  }

  def getUsedRegs(): List[Register] = usedReg.toList

}
