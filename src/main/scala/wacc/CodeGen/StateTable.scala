package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._

class StateTable(stateT: Option[StateTable]) {

  /* Parameter Information */
  val paramDictionary: mutable.Map[String, Location] = mutable.Map[String, Location]()
  val usedParamReg = mutable.ListBuffer[Register]() ++= (stateT match {
    case Some(upSt) => upSt.getUsedParamRegs()
    case None => List()
  })
  var paramPtr: Int = stateT match {
    case Some(upSt) => upSt.paramPtr
    case None => 0
  }

  /* Variable Information */
  val encStateTable = stateT
  val dictionary    = mutable.Map[String, Location]()
  val usedReg: mutable.ListBuffer[Register] =
    encStateTable match {
      case Some(upSt) => mutable.ListBuffer[Register]() ++= upSt.getUsedRegs()
      case None => mutable.ListBuffer[Register]()
    }
  
  /* Saved Registers Information */
  var savedRegs = stateT match {
    case Some(upSt) => upSt.getSavedRegs()
    case None       => List()
  }
  var varNum = stateT match {
    case Some(upSt) => upSt.getVarNum()
    case None       => 0
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

  def addParam(name: String, location: Location) = {
    paramDictionary += (name -> location)

    // add used register for parameter 
    location match {
      case location: Register => usedParamReg += location
      case _             => updateParamPtr(nextParamPtr())
    }
  }

  def modifySavedRegs(regs: Seq[Register]) = {
    savedRegs = regs
  }

  def modifyVarNum(num: Int) = {
    varNum = num
  }

  def updateParam(name: String, location: Location) = {
    paramDictionary(name) = location
  }

  /* Look up a value according to key in this symbol table */
  def lookUp(name: String): Option[Location] = {
    // First search in variable
    val result = dictionary.get(name)

    // If cannot find, search in parameter
    result match {
      case Some(_) => result
      case None => paramDictionary.get(name)
    }
  }
    

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

  def updateParamPtr(num: Int) =
    paramPtr = num

  private def nextParamPtr() = paramPtr - 4

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

  /* Return the next location for parameter storage */
  def nextParamLocation(): Location = {

    // Find available register first
    val unUsedReg = paramReg.filterNot(usedParamReg.toSet)

    // If has unused reg
    if (!unUsedReg.isEmpty) {
      unUsedReg.head
    } else {
      // Go to storage
      val returnLoc = RegIntOffset(FP, paramPtr)
      returnLoc
    }
  }

  // When function call inside a function with parameter
  // we push the parameters to stack to allow caller saved register
  def updateParamToStack() = {
    for (key_value <- paramDictionary) {
      (key_value) match {
        case (name, R0) => updateParam(name, RegIntOffset(R12, 0)) 
        case (name, R1) => updateParam(name, RegIntOffset(R12, 4))
        case (name, R2) => updateParam(name, RegIntOffset(R12, 8))
        case _ =>
      }
    }
  }

  // After calling, R0, R1, R2 should be changed back
  def updateParamBackToReg() = {
    for (key_value <- paramDictionary) {
      (key_value) match {
        case (name, RegIntOffset(R12, 0)) => updateParam(name, R0)
        case (name, RegIntOffset(R12, 4)) => updateParam(name, R1)
        case (name, RegIntOffset(R12, 8)) => updateParam(name, R2)
        case _ =>
      }
    }
  }

  def getUsedRegs(): Seq[Register] = usedReg.toSeq

  def getUsedParamRegs(): Seq[Register] = usedParamReg.toSeq

  def getSavedRegs(): Seq[Register] = savedRegs.toSeq

  def getVarNum(): Int = varNum
  
}
