package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr
import ExprSemantic._
import ValueSemantic._
import SymbolObject._
import SymbolObjectType._

object StatSemantic {
  def checkStat(stat: Stat, st: SymbolTable): Boolean = {
    stat match {
      case Skip() =>
      /* check for declaration: what object is created? */
      case Declare(type1, ident, initValue) => declareCheck(type1, ident, initValue, st)
      case Assign(target, newValue) => assignCheck(target, newValue, st)
      case Read(lvalue) => readCheck(lvalue, st)
      case Free(expr) => freeCheck(expr, st)
      case Return(expr) => returnCheck(expr, st)
      case Exit(expr) => exitCheck(expr, st)
      case Print(expr) => checkExpr(expr, st)
      case Println(expr) => checkExpr(expr, st)
      case If(expr, stat1, stat2) => ifCheck(expr, stat1, stat2, st)
      case While(expr, stat) => whileCheck(expr, stat, st)
      case Begin(stat) => beginCheck(stat, st)
    }
    true
  }

  /* Name must not clash keywords and other variables in scope 
     Initial value type should match type of variable*/
  def declareCheck(type1: Types.Type, ident: Ident, initValue: Rvalue, st: SymbolTable): Unit = {
    val semType: Type = convertType(type1)
    /* Check existence, Create new VariableObj */
    st.lookUp(ident.name, VariableType()) match {
      case Some(VariableObj(_)) => semanticErr("Declare: Variable name already exists")
      case Some(ParamObj(_)) => {
        st.remove(ident.name, VariableType())
        st.add(ident.name, VariableType(), VariableObj(semType))
      }
      case _ => st.add(ident.name, VariableType(), VariableObj(semType))
    }

    /* Initial value should match the type */
    if (!equalType(semType, checkRvalue(initValue, st))) {
      semanticErr("Declare: Initial value wrong type")
    }
  }

  /* Convert syntax type to semantics type */
  def convertType(syntaxType: Types.Type): Type = {
    syntaxType match {
      case Types.IntType()  => IntType()
      case Types.BoolType() => BoolType()
      case Types.CharType() => CharType()
      case Types.StrType()  => StrType()
      case Types.PairType(t1, t2) => PairType(convertPairElemType(t1), convertPairElemType(t2))
      case Types.ArrayType(t) => ArrayType(convertType(t))
    }
  }

  /* Convert special syntax type - pair elem type - to semantics type */
  def convertPairElemType(syntaxType: Types.PairElemType): Type = {
    syntaxType match {
      case Types.IntType()  => IntType()
      case Types.BoolType() => BoolType()
      case Types.CharType() => CharType()
      case Types.StrType()  => StrType()
      case Types.PairTypeIdent()  => PairType(AnyType(), AnyType())
      case Types.ArrayType(t) => ArrayType(convertType(t))
    }
  }

  def assignCheck(target: Lvalue, newValue: Rvalue, st: SymbolTable): Unit = {
    newValue match {
      case newValue: Expr => retCheck(target, newValue, st)
      case newValue: ArrayLit => retCheck(target, newValue, st)
      case newValue: Call => retCheck(target, newValue, st)
      case newValue: NewPair => retCheck(target, newValue, st)
      case newValue: PairElem => retCheck(target, newValue, st)
      case _ => semanticErr("Assign: wrong target type")
    }
  }

  /* Target type match newValue type */
  def retCheck(target: Lvalue, newValue: Rvalue, st: SymbolTable) = {
    if (!equalType(checkLvalue(target, st), checkRvalue(newValue, st))) {
      semanticErr("Assign: assign value mismatch target ")
    }
  }
  
  /* Special assignment:
     Input from standard input: String
     Target type: Int / Char */
  def readCheck(target: Lvalue, st: SymbolTable): Unit = {
    checkLvalue(target, st) match {
      case IntType() => 
      case CharType() => 
      case _ => semanticErr("Read: target not int or char")
    }
  }

  /* Target type: pair / array
     free is not recursive */
  def freeCheck(expr: Expr, st: SymbolTable): Unit = {
    checkExpr(expr, st) match {
      case PairType(_, _) => 
      case ArrayType(_) => 
      case _ => semanticErr("Free: target not pair or array")
    }
  }

  /* Need be in a non-main function
     Expr type should match return type of function */
  def returnCheck(expr: Expr, st: SymbolTable): Unit = {
    if (!equalType(findFuncRetType(st), checkExpr(expr, st))) {
      semanticErr("Return: not matching return type")
    }
  }

  /* Find the return type of the function in scope of st */
  def findFuncRetType(st: SymbolTable): Type = {
    var retType: Type = null
    var recurse_st: SymbolTable = st

    while(recurse_st.encSymTable != null) {
      retType = findOneRet(st)

      if (retType != null) {
        return retType
      } else {
        recurse_st = recurse_st.encSymTable
      }
    }

    semanticErr("Return: no function in scope")
  }

  /* Recursive return finding */
  def findOneRet(st: SymbolTable): Type = {
    var retType: Type = null

    for ((name, obj) <- st.dictionary) {
      /* find function in this scope */
      obj match {
        case obj: FuncObj => retType = obj.returnType
        case _ =>
      }
    }
    
    retType
  }

  /* Can be in body of any function
     Arg type: Int */
  def exitCheck(expr: Expr, st: SymbolTable): Unit = {
    if (checkExpr(expr, st) != IntType()) {
      semanticErr("Exit: arg not int")
    }
  }

  /* Expr type: Bool
     Check validity of stat1, stat2 */
  def ifCheck(expr: Expr, stat1: List[Stat], 
              stat2: List[Stat], st: SymbolTable): Unit = {
    if (checkExpr(expr, st) != BoolType()) {
      semanticErr("If: condition not bool")
    }
    val new_st1 = new SymbolTable(st)
    stat1.foreach{s => checkStat(s, new_st1)}

    val new_st2 = new SymbolTable(st)
    stat2.foreach{s => checkStat(s, new_st2)}
  }

  /* Expr type: Bool
     Check validity of stat */
  def whileCheck(expr: Expr, stat: List[Stat], st: SymbolTable): Unit = {
    if (checkExpr(expr, st) != BoolType()) {
      semanticErr("If: condition not bool")
    }

    val new_st = new SymbolTable(st)
    stat.foreach{s => checkStat(s, new_st)}
  }

  /* Check validity of stat 
     Start new scope */
  def beginCheck(stat: List[Stat], st: SymbolTable): Unit = {
    val new_st = new SymbolTable(st)
    stat.foreach{s => checkStat(s, new_st)}
  }

  /* Type equality involve AnyType */
  def equalType(type1: Type, type2: Type): Boolean = {
    if (type1 == type2) {
      return true
    }

    (type1, type2) match {
      case (PairType(_, _), PairType(AnyType(), AnyType())) => return true
      case (PairType(AnyType(), AnyType()), PairType(_, _)) => return true
      case (ArrayType(_), ArrayType(AnyType())) => return true
      case (ArrayType(AnyType()), ArrayType(_)) => return true
      case _ => return false
    }
  }

}