package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr
import ExprSemantic._
import ValueSemantic._
import SymbolObject._

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
    st.lookUp(ident.name) match {
      case Some(_) => semanticErr("Declare: Variable name already exists")
      case None => st.add(ident.name, VariableObj(semType))
    }

    /* Initial value should match the type */
    if (semType != checkRvalue(initValue, st)) {
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
    if (checkLvalue(target, st) != checkRvalue(newValue, st)) {
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
    if (findFuncRetType(st) != checkExpr(expr, st)) {
      semanticErr("Return: not matching return type")
    }
  }

  /* Find the return type of the function in scope of st */
  def findFuncRetType(st: SymbolTable): Type = {
    var retType: Type = null
    var found = false

    for ((name, obj) <- st.dictionary) {
      /* Cannot return from main function */
      if (name == "main") {
        semanticErr("Return: return in main function")
      }
      /* find function in this scope */
      obj match {
        case obj: FuncObj => {
          retType = obj.returnType
          found = true
        }
        case _ =>
      }
    }
    /* if not in function body, fail */
    if (!found) {
      semanticErr("Return: no function found in st")
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
    stat1.foreach{s => checkStat(s, st)}
    stat2.foreach{s => checkStat(s, st)}
  }

  /* Expr type: Bool
     Check validity of stat */
  def whileCheck(expr: Expr, stat: List[Stat], st: SymbolTable): Unit = {
    if (checkExpr(expr, st) != BoolType()) {
      semanticErr("If: condition not bool")
    }
    stat.foreach{s => checkStat(s, st)}
  }

  /* Check validity of stat */
  def beginCheck(stat: List[Stat], st: SymbolTable): Unit = {
    stat.foreach{s => checkStat(s, st)}
  }

}
