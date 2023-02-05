package wacc

import SymbolObject._
import Ast._
import SemanticType._

object SemanticChecker {

  var st: SymbolTable = new SymbolTable(null)

  def checkExpr(expr: Expr, st: SymbolTable): Type = {
    expr match {
      /* Arithmetic binary operators */
      case Mul(e1, e2) => arithmeticsCheck(e1, e2, st)
      case Div(e1, e2) => arithmeticsCheck(e1, e2, st)
      case Mod(e1, e2) => arithmeticsCheck(e1, e2, st)
      case Add(e1, e2) => arithmeticsCheck(e1, e2, st)
      case Sub(e1, e2) => arithmeticsCheck(e1, e2, st)
      
      /* Comparison binary operators */
      case Gt (e1, e2) => compareCheck(e1, e2, st)
      case Gte(e1, e2) => compareCheck(e1, e2, st)
      case Lt (e1, e2) => compareCheck(e1, e2, st)
      case Lte(e1, e2) => compareCheck(e1, e2, st)

      /* Equality binary operators */
      case Eq (e1, e2) => eqCheck(e1, e2, st)
      case Neq(e1, e2) => eqCheck(e1, e2, st)

      /* Logical binary operators */
      case And(e1, e2) => logiCheck(e1, e2, st)
      case Or (e1, e2) => logiCheck(e1, e2, st)

      /* Unary operators */
      case Not(e) => unaryCheck(e, st, BoolType(), BoolType())
      case Neg(e) => unaryCheck(e, st, IntType(), IntType())
      case Len(e) => lenCheck(e, st)
      case Ord(e) => unaryCheck(e, st, CharType(), IntType())
      case Chr(e) => unaryCheck(e, st, IntType(), CharType())

      /* Literals */
      case IntLit(_)  => IntType()
      case BoolLit(_) => BoolType()
      case CharLit(_) => CharType()
      case StrLit(_)  => StrType()
      case PairLit()  => PairType(AnyType(), AnyType())

      case Ident(name) => identCheck(name, st)
      case ArrayElem(ident, indexes) => arrayElemCheck(ident, indexes, st)
    }
  }

  def checkLvalue(lvalue: Lvalue, st: SymbolTable): Type = {
    lvalue match {
      case lvalue: Expr => checkExpr(lvalue, st)
      case lvalue: Rvalue => checkRvalue(lvalue, st)
    }
  }

  def checkRvalue(rvalue: Rvalue, st: SymbolTable): Type = { 
    rvalue match {
      case NewPair(e1, e2) => newPairCheck(e1, e2, st)
      case Call(ident, args) => callCheck(ident, args, st)
      case PairElem(index, lvalue) => pairElemCheck(index, lvalue, st)
      case ArrayLit(values) => arrayLitCheck(values, st)
      case rvalue: Expr => checkExpr(rvalue, st)
    }
  }

  def checkState(stat: Stat, st: SymbolTable): Boolean = {
    stat match {
      /* check for declaration: what object is created? */
      case Declare(type1, ident, initValue) => declareCheck(type1, ident, initValue, st)
      case Assign(target, newValue) => assignCheck(target, newValue, st)
      case _ => false
    }
    true
  }





  def semanticErr(where: String) = {
    throw new SemanticErr("Semantic Error in " + where)
  }

  case class SemanticErr(private val message: String = "Semantic Error", 
                         private val cause: Throwable = None.orNull)
                         extends Exception(message, cause) 

  /* Argument1 type: Int,
     Argument2 type: Int,
     Return type   : Int */
  def arithmeticsCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (checkExpr(expr1, st) == IntType()) {
        if (checkExpr(expr2, st) == IntType()) {
          /* return type */
          IntType()
        } else {
          semanticErr("ArithBio: Expr2 not int type")
        }
      } else {
        semanticErr("ArithBio: Expr1 not int type")
    }
  }

  /* Argument1 type: Int/Char,
     Argument2 type: Same as Arg1,
     Return type   : Bool */
  def compareCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (checkExpr(expr1, st) == checkExpr(expr2, st)) {
      if (checkExpr(expr1, st) == IntType() || checkExpr(expr2, st) == CharType()) {
        /* return type */
        BoolType()
      } else {
        semanticErr("CompBio: Both expressions are not int or char type")
      }
    } else {
      semanticErr("CompBio: Both expressions are not of the same type")
    }
  }

  /* Argument1 type: T,
     Argument2 type: T,
     Return type   : Bool */
  def eqCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (checkExpr(expr1, st) == checkExpr(expr2, st)) {
      BoolType()
    } else {
      semanticErr("EqBio: Both expressions are not of the same type")
    }
  }

  /* Argument1 type: Bool,
     Argument2 type: Bool,
     Return type   : Bool */
  def logiCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (checkExpr(expr1, st) == BoolType()) {
        if (checkExpr(expr2, st) == BoolType()) {
          /* return type */
          BoolType()
        } else {
          semanticErr("LogiBio: Expr2 not bool type")
        }
      } else {
        semanticErr("LogiBio: Expr1 not bool type")
    }
  }

  /* Arg type: Bool
     Return type: Bool */
  def unaryCheck(expr: Expr, st: SymbolTable, argType: Type, retType: Type): Type = {
    if (checkExpr(expr, st) != argType) {
          semanticErr(s"Unary: argument not $argType")
      }
    retType
  }

  /* Arg type: T[]
     Return type: Int */
  def lenCheck(expr: Expr, st: SymbolTable): Type = {
    checkExpr(expr, st) match {
      case ArrayType(_) => 
      case _ => semanticErr("Len: argument not array")
    }
    IntType()
  }

  /* refer to st */
  def identCheck(name: String, st: SymbolTable): Type = {
    st.lookUp(name) match {
      case Some(symObj) => symObj.getType()
      case None => semanticErr("Ident: not in symbol table")
    }
  }

  /* Arg1: Ident -> Refer to ArrayObj in st -> T[]
     Arg2: Int[] -> every element Int
     Return: T */
  def arrayElemCheck(ident: Ident, indexes: List[Expr], st: SymbolTable): Type = {
    var returnType: Type = null

    /* First argument type should be T[] */
    st.lookUp(ident.name) match {
      case Some(symObj) => { 
        symObj match {
        /* Return type should be T */
        case symObj: ArrayObj => returnType = symObj.elemType
        case _ => semanticErr("ArrayElem: fst arg not arrayObj")
      }
    }
      case None => semanticErr("ArrayElem: not in symbol table")
    }

    /* Second argument should be Int */
    indexes.foreach{i => if (checkExpr(i, st) != IntType()) 
                            { semanticErr("ArrayElem: index not int type")}}

    returnType
  }

  /* Arg1: PairElementType
     Arg2: PairElementType
     Return: PairType(Arg1Type, Arg2Type) */
  def newPairCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = { 
    val type1 = checkExpr(expr1, st)
    val type2 = checkExpr(expr2, st)
    PairType(type1, type2)
  }

  /* Arg1: Ident -> Refer to FuncObj in st
     FuncObj(returnType, args, argc, st) 
     Arg2: args -> type match to FuncObj(args)
     Return: FuncObj(returnType) */
  def callCheck(ident: Ident, args: List[Expr], st: SymbolTable): Type = { 
    var funcObj: FuncObj = null
    /* Find funcObj */
    st.lookUpAll(ident.name) match {
      case Some(symObj) =>  { 
          symObj match {
          case symObj: FuncObj => funcObj = symObj
          case _ => semanticErr("ArrayElem: fst arg not funcObj")
        }
      }
      case None => semanticErr("Call: function name not in symbol table")
    }

    /* check number of parameters */
    if (args.length != funcObj.argc) {
      semanticErr("Call: wrong argument number")
    }

    /* check every parameter's type */
    for (i <- 0 until args.length) {
      checkValueRef(args(i), st)
      if (checkExpr(args(i), st) != funcObj.args(i).getType()) {
        semanticErr("Call: function name not in symbol table")
      }
    }

    /* Return type */
    funcObj.returnType
  }

  /* Check pass parameter by value or by ref */
  def checkValueRef(expr: Expr, st: SymbolTable) = {
    expr match {
      // Int, Bool, Char should by value
      case IntLit(_) | BoolLit(_) | CharLit(_)  =>
      // String, Array, Pair should by reference
      case Ident(_) => checkExpr(expr, st) match {
        case StrType() =>
        case ArrayType(_) =>
        case PairType(_, _) =>
        case _ => semanticErr("Call: wrong by value Or by reference")
      }
      case _ => semanticErr("Call: wrong by value Or by reference")
    }
  }

  /* Arg1: fst/snd
     Arg2: PairType(T1, T2)
     Return: if fst -> T1; if snd -> T2 */
  def pairElemCheck(index: String, lvalue: Lvalue, st: SymbolTable): Type = { 
    var returnType: Type = null

    /* Lvalue should be a pair */
    val lType = checkLvalue(lvalue, st)
    lType match {
      case PairType(t1, t2) => index match {
        case "fst" => returnType = t1
        case "snd" => returnType = t2
      }
      case _ => semanticErr("Pair elem: not Pair Type")
    }

    returnType
  }

  /* Arg: All elements in List same type T
     Return: ArrayType(T)
     OR
     Arg: [] -> empty list
     Return: ArrayType(AnyType) */
  def arrayLitCheck(values: List[Expr], st: SymbolTable): Type = { 
    /* check empty */
    if (values.length == 0) {
      ArrayType(AnyType())
    }
    
    /* check every parameter's type */
    for (i <- 0 until values.length - 1) {
      // checkValueRef(args(i), st)
      if (checkExpr(values(i), st) != checkExpr(values(i + 1), st)) {
        semanticErr("ArrayLit: Array literals are not of the same type")
      }
    }
    ArrayType(checkExpr(values(0), st))
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
  
  
  
}
