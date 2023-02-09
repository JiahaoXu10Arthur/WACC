package wacc
import Ast._
import SemanticType._
import SymbolObject._
import SymbolObjectType._
import scala.collection.mutable.ListBuffer
import Errors._
import SemanticErrorBuilder._

object ExprSemantic {

  def checkExpr(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    expr match {
      /* Arithmetic binary operators */
      case Mul(e1, e2) => arithmeticsCheck(e1, e2)
      case Div(e1, e2) => arithmeticsCheck(e1, e2)
      case Mod(e1, e2) => arithmeticsCheck(e1, e2)
      case Add(e1, e2) => arithmeticsCheck(e1, e2)
      case Sub(e1, e2) => arithmeticsCheck(e1, e2)

      /* Comparison binary operators */
      case Gt(e1, e2)  => compareCheck(e1, e2)
      case Gte(e1, e2) => compareCheck(e1, e2)
      case Lt(e1, e2)  => compareCheck(e1, e2)
      case Lte(e1, e2) => compareCheck(e1, e2)

      /* Equality binary operators */
      case Eq(e1, e2)  => eqCheck(e1, e2)
      case Neq(e1, e2) => eqCheck(e1, e2)

      /* Logical binary operators */
      case And(e1, e2) => logiCheck(e1, e2)
      case Or(e1, e2)  => logiCheck(e1, e2)

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

      case expr: Ident               => identCheck(expr)
      case ArrayElem(ident, indexes) => arrayElemCheck(ident, indexes)
    }
  }

  /* Argument1 type: Int,
     Argument2 type: Int,
     Return type   : Int */
  def arithmeticsCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    if (!equalType(type1, IntType())) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(IntType()),
        Seq(" First expression is not int ")
      )
    }

    val type2 = checkExpr(expr2)
    if (!equalType(type2, IntType())) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(IntType()),
        Seq(" Second expression is not int ")
      )
    }

    IntType()
  }

  /* Argument1 type: Int/Char,
     Argument2 type: Same as Arg1,
     Return type   : Bool */
  def compareCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)

    if (!equalType(type1, type2)) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(type1),
        Seq(" Expressions are not the same type ")
      )
    }

    if (!equalType(type1, IntType()) && !equalType(type1, CharType())) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(IntType(), CharType()),
        Seq(" First expression is not int nor char")
      )
    }

    if (!equalType(type2, IntType()) && !equalType(type2, CharType())) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(IntType(), CharType()),
        Seq(" Second expression is not int nor char")
      )
    }

    BoolType()
  }

  /* Argument1 type: T,
     Argument2 type: T,
     Return type   : Bool */
  def eqCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)

    if (!equalType(type1, type2)) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(type1),
        Seq(" Expressions are not the same type ")
      )
    }
    BoolType()
  }

  /* Argument1 type: Bool,
     Argument2 type: Bool,
     Return type   : Bool */
  def logiCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)

    if (!equalType(type1, BoolType())) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(BoolType()),
        Seq(" First expression is not bool ")
      )
    }

    if (!equalType(type2, BoolType())) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(BoolType()),
        Seq(" Second expression is not bool ")
      )
    }

    BoolType()
  }

  /* Arg type: given
     Return type: given */
  def unaryCheck(expr: Expr, argType: Type, retType: Type)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr)
    if (!equalType(type1, argType)) {
      semErr += buildTypeError(
        expr.pos,
        type1,
        Set(retType),
        Seq(s" Expression is not $argType ")
      )
    }
    retType
  }

  /* Arg type: T[]
     Return type: Int */
  def lenCheck(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    val type1 = checkExpr(expr)
    checkExpr(expr) match {
      case AnyType()    =>
      case ArrayType(_) =>
      case _ =>
        semErr += buildTypeError(
          expr.pos,
          type1,
          Set(ArrayType(AnyType())),
          Seq(" Expression is not array ")
        )
    }
    IntType()
  }

  /* refer to st */
  def identCheck(
      ident: Ident
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    st.lookUpAll(ident.name, VariableType()) match {
      case Some(symObj) => symObj.getType()
      case None => {

        semErr += buildScopeError(
          ident.pos,
          ident.name,
          st.lookUpAllSimilar(ident.name, VariableType()),
          Seq(s" Variable ${ident.name} has not been declared in this scope ")
        )
        st.add(
          ident.name,
          VariableType(),
          new VariableObj(AnyType(), ident.pos)
        )
        AnyType()
      }
    }

  }

  /* Arg1: Ident -> Refer to ArrayObj in st -> T[]
     Arg2: Int[] -> every element Int
     Return: T */
  def arrayElemCheck(ident: Ident, indexes: List[Expr])(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val exprType = checkExpr(ident)

    /* Check index is int */
    var index = 0
    var error = false
    while (index < indexes.length && !error) {
      val indexType = checkExpr(indexes(index))
      indexType match {
        case IntType() =>
        case _ => {
          semErr += buildTypeError(
                      indexes(index).pos,
                      indexType,
                      Set(IntType()),
                      Seq(" The index of an array needs to be int type ")
                      )
          error = true
          return AnyType()
        }
      }
      index += 1
    }

    /* Check correct dimension */
    var returnType = exprType
    index = 0
    while (index < indexes.length && !error) {
      returnType match {
        case AnyType() =>
        case ArrayType(elemType) => returnType = elemType
        case other => {
          val shouldType = createNestArrayType(other, indexes.length)
          semErr += buildTypeError(
            ident.pos,
            exprType,
            Set(shouldType),
            Seq(" Incorrect array dimension ")
          )
          error = true
          shouldType
        }
      }

      index += 1
    }

    returnType
  }

  
  def createNestArrayType(innerType: Type, indexNum: Int): Type = {
    var returnType = innerType
    for (i <- 0 until indexNum) {
      returnType = ArrayType(returnType)
    }
    returnType
  }

}
