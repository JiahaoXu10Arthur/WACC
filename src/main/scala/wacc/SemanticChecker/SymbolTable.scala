package wacc.SemanticChecker

import scala.collection.mutable

import SymbolObject._
import SymbolObjectType._
import SemanticTypes._

class SymbolTable(st: SymbolTable, tableType: SymbolObjectType.ObjectType) {

  val dictionary = mutable.Map[(String, ObjectType), List[SymbolObj]]()
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
  def add(name: String, objType: ObjectType, obj: SymbolObj) = {
    val objs = mutable.ListBuffer[SymbolObj]()

    objType match {
      case FunctionType() => {
        /* Add possible overloading functions to list */
        lookUpFunc(name) match {
          case Some(existObjs) => objs ++= existObjs
          case _ =>
        }
      }
      case _ => 
    }

    // Add new pair
    objs += obj

    // Add symbol name-object pair to dictionary
    dictionary += ((name, objType) -> objs.toList)
  }
    

  /* Remove a key-value pair specified by key from dictionary */
  def remove(name: String, objType: ObjectType) =
    dictionary -= ((name, objType))

  /* Look up a variable value according to key in this symbol table */
  /* Use .head because variable cannot be redefined, so only 1 obj per identifier */
  def lookUpVar(name: String): Option[SymbolObj] =
    dictionary.get((name, VariableType())) match {
      case Some(objs) => Some(objs.head)
      case None       => None
    }

  /* Look up a value according to key in this symbol table and all parent table*/
  def lookUpAllVar(name: String): Option[SymbolObj] = {
    var s = this
    while (s != null) {
      val obj = s.lookUpVar(name)
      if (obj != None) {
        return obj
      }
      s = s.encSymTable
    }

    None
  }

  /* Look up a function according to key in this symbol table and all parent table*/
  /* Function may have overloading, so return a list of function objects */
  def lookUpFunc(name: String): Option[List[SymbolObj]] =
    dictionary.get((name, FunctionType()))

  /* Look up a value according to key in this symbol table and all parent table*/
  def lookUpAllFunc(name: String): Option[List[SymbolObj]] = {
    // Can only define function in main scope

    var mainSt = this
    while (mainSt.encSymTable != null) {
      mainSt = mainSt.encSymTable
    }

    mainSt.lookUpFunc(name)
  }

  private def correctFuncObj(func: FuncObj,
                                   expectedRet: Type, 
                                   expectedArgs: List[Type]): Boolean = {
    val funcRet  = func.returnType
    val funcArgs = func.args.map(_.t)
    if (sameFunction(expectedRet, funcRet, expectedArgs, funcArgs)) {
      true
    } else {
      false
    }
  }

  /* Find the overload function object with the same argument types */
  def getOverloadFuncObj(name: String,
                         expectRet:  Type,
                         expectArgs: List[Type]): Option[FuncObj] = {
    // Get all overloading functions of this name
    val funcObjs = lookUpAllFunc(name)
    var retObj: Option[FuncObj] = None

    funcObjs match {
      case Some(funcs) => {
        /* For each overloading function, check if the argument types are the same */
        for (func <- funcs) { 
          func match {
            case func: FuncObj => {
              if (correctFuncObj(func, expectRet, expectArgs))
                retObj = Some(func)
            }
            case _ =>
          }
        }
      }
      case None =>
    }

    retObj
  }

  /* Get the overload index of a function */
  private def getOverloadFuncIndex(name: String, 
                           expectRet: Type, 
                           expectArgs: List[Type]): Int = {
    // Get all overloading functions of this name
    val funcObjs = lookUpAllFunc(name)
    var retIndex = -1

    funcObjs match {
      case Some(funcs) => {
        var index = 0
        /* For each overloading function, check if the argument types are the same */
        for (func <- funcs) {
          func match {
            case func: FuncObj => {
              if (correctFuncObj(func, expectRet, expectArgs))
                retIndex = index
            }
            case _ =>
          }
          index += 1
        }
      }
      case None =>
    }

    retIndex
  }

// Get overload function name with index
def getOverloadFuncName(baseFuncName: String, 
                        expectRet: Type, 
                        expectArgs: List[Type]): String = {
  baseFuncName + getOverloadFuncIndex(baseFuncName, expectRet, expectArgs)                         
}

  // find variable number in this scope
  private def findVarNum(): Int = {
    var var_num = 0
    
    for (key_value <- dictionary) {
      key_value._2.head match {
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
          similar += ((x._1._1, x._2.head.getPos()))
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
  def getImmutableTable(): (Map[(String, ObjectType), List[SymbolObj]], 
                            List[SymbolTable]) = {
    (dictionary.toMap, subSts.toList)
  }
}
