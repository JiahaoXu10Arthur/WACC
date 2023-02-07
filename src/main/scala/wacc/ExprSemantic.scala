package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr
import SymbolObjectType._
import scala.collection.mutable.ListBuffer
import Errors._

object ExprSemantic {

	def checkExpr(expr: Expr)
               (implicit st: SymbolTable, 
                         semErr: ListBuffer[WACCError]): Type = {
    expr match {
      /* Arithmetic binary operators */
      case Mul(e1, e2) => arithmeticsCheck(e1, e2)
      case Div(e1, e2) => arithmeticsCheck(e1, e2)
      case Mod(e1, e2) => arithmeticsCheck(e1, e2)
      case Add(e1, e2) => arithmeticsCheck(e1, e2)
      case Sub(e1, e2) => arithmeticsCheck(e1, e2)
      
      /* Comparison binary operators */
      case Gt (e1, e2) => compareCheck(e1, e2)
      case Gte(e1, e2) => compareCheck(e1, e2)
      case Lt (e1, e2) => compareCheck(e1, e2)
      case Lte(e1, e2) => compareCheck(e1, e2)

      /* Equality binary operators */
      case Eq (e1, e2) => eqCheck(e1, e2)
      case Neq(e1, e2) => eqCheck(e1, e2)

      /* Logical binary operators */
      case And(e1, e2) => logiCheck(e1, e2)
      case Or (e1, e2) => logiCheck(e1, e2)

      /* Unary operators */
      case Not(e) => unaryCheck(e, BoolType(), BoolType())
      case Neg(e) => unaryCheck(e, IntType(), IntType())
      case Len(e) => lenCheck(e)
      case Ord(e) => unaryCheck(e, CharType(), IntType())
      case Chr(e) => unaryCheck(e, IntType(), CharType())

      /* Literals */
      case IntLit(_)  => IntType()
      case BoolLit(_) => BoolType()
      case CharLit(_) => CharType()
      case StrLit(_)  => StrType()
      case PairLit()  => PairType(AnyType(), AnyType())

      case Ident(name) => identCheck(name)
      case ArrayElem(ident, indexes) => arrayElemCheck(ident, indexes)
    }
  }

  /* Argument1 type: Int,
     Argument2 type: Int,
     Return type   : Int */
  def arithmeticsCheck(expr1: Expr, expr2: Expr)
                      (implicit st: SymbolTable, 
                                semErr: ListBuffer[WACCError]): Type = {
    if (checkExpr(expr1) != IntType()) {
      semanticErr("ArithBio: Expr1 not int type")
    }

    if (checkExpr(expr2) != IntType()) {
      semanticErr("ArithBio: Expr2 not int type")
    }

    IntType()
  }

  /* Argument1 type: Int/Char,
     Argument2 type: Same as Arg1,
     Return type   : Bool */
  def compareCheck(expr1: Expr, expr2: Expr)
                  (implicit st: SymbolTable, 
                            semErr: ListBuffer[WACCError]): Type = {
    if (!equalType(checkExpr(expr1), checkExpr(expr2))) {
      semanticErr("CompBio: expressions are not of the same type")
    }

    if (checkExpr(expr1) != IntType() && checkExpr(expr1) != CharType()) {
      semanticErr("CompBio: expr1 is not int or char type")
    }

    if (checkExpr(expr2) != IntType() && checkExpr(expr1) != CharType()) {
      semanticErr("CompBio: expr2 is not int or char type")
    }
    
    BoolType()
  }

  /* Argument1 type: T,
     Argument2 type: T,
     Return type   : Bool */
  def eqCheck(expr1: Expr, expr2: Expr)
             (implicit st: SymbolTable, 
                       semErr: ListBuffer[WACCError]): Type = {
     if (!equalType(checkExpr(expr1), checkExpr(expr2))){
      semanticErr("EqBio: Both expressions are not of the same type")
    }
    BoolType()
  }

  /* Argument1 type: Bool,
     Argument2 type: Bool,
     Return type   : Bool */
  def logiCheck(expr1: Expr, expr2: Expr)
               (implicit st: SymbolTable, 
                         semErr: ListBuffer[WACCError]): Type = {
    if (checkExpr(expr1) != BoolType()) {
      semanticErr("LogiBio: Expr1 not bool type")
    }
    
    if (checkExpr(expr2) != BoolType()) {
      semanticErr("LogiBio: Expr2 not bool type")
    }

    BoolType()
  }

  /* Arg type: Bool
     Return type: Bool */
  def unaryCheck(expr: Expr, argType: Type, retType: Type)
                (implicit st: SymbolTable, 
                          semErr: ListBuffer[WACCError]): Type = {
    if (checkExpr(expr) != argType) {
          semanticErr(s"Unary: argument not $argType")
    }
    retType
  }

  /* Arg type: T[]
     Return type: Int */
  def lenCheck(expr: Expr)
              (implicit st: SymbolTable, 
                        semErr: ListBuffer[WACCError]): Type = {
    checkExpr(expr) match {
      case ArrayType(_) => 
      case _ => semanticErr("Len: argument not array")
    }
    IntType()
  }

  /* refer to st */
  def identCheck(name: String)
                (implicit st: SymbolTable,
                          semErr: ListBuffer[WACCError]): Type = {
    st.lookUpAll(name, VariableType()) match {
      case Some(symObj) => symObj.getType()
      case None => {
        semanticErr(s"Ident: $name not in symbol table")
        AnyType()
      }
    }
    
  }

  /* Arg1: Ident -> Refer to ArrayObj in st -> T[]
     Arg2: Int[] -> every element Int
     Return: T */
  def arrayElemCheck(ident: Ident, indexes: List[Expr])
                    (implicit st: SymbolTable, 
                              semErr: ListBuffer[WACCError]): Type = {
    var thisLayer = checkExpr(ident)

    /* for every layer of index, check validity */
    for (i <- indexes) {
      thisLayer = oneArrayElemCheck(thisLayer, i)
    }

    thisLayer
  }

  /* One layer of array elem check */
  def oneArrayElemCheck(t: Type, index: Expr)
                       (implicit st: SymbolTable, 
                                 semErr: ListBuffer[WACCError]): Type = {
    if (checkExpr(index) != IntType()) {
      semanticErr("ArrayElem: index not int type")
      return AnyType()
    }

    /* First argument type should be T[] */
    t match {
      case ArrayType(t1) => return t1
      case _ => {
        semanticErr("ArrayElem: fst arg not arrayObj")
        return AnyType()
      }
    }
  }

}

