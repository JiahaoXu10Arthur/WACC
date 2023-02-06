package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr

object ExprSemantic {

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
    st.lookUpAll(name) match {
      case Some(symObj) => symObj.getType()
      case None => semanticErr("Ident: not in symbol table")
    }
  }

  /* Arg1: Ident -> Refer to ArrayObj in st -> T[]
     Arg2: Int[] -> every element Int
     Return: T */
  def arrayElemCheck(ident: Ident, indexes: List[Expr], st: SymbolTable): Type = {
    var thisLayer = checkExpr(ident, st)

    for (i <- indexes) {
      thisLayer = oneArrayElemCheck(thisLayer, i, st)
    }

    thisLayer
  }

  /* One layer of array elem check */
  def oneArrayElemCheck(t: Type, index: Expr, st: SymbolTable): Type = {
    if (checkExpr(index, st) != IntType()) {
      semanticErr("ArrayElem: index not int type")
    }

    /* First argument type should be T[] */
    t match {
      case ArrayType(t1) => return t1
      case _ => semanticErr("ArrayElem: fst arg not arrayObj")
    }
  }

}

