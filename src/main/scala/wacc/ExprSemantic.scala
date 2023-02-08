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
    if (type1 != IntType()) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(IntType()),
        Seq("ArithBio: Expr1 not int type")
      )
    }

    val type2 = checkExpr(expr2)
    if (type2 != IntType()) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(IntType()),
        Seq("ArithBio: Expr2 not int type")
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
        Seq("ArithBio: expressions not same type")
      )
    }

    if (type1 != IntType() && type1 != CharType()) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(IntType(), CharType()),
        Seq("ArithBio: Expr1 not int or char type")
      )
    }

    if (type2 != IntType() && type2 != CharType()) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(IntType(), CharType()),
        Seq("ArithBio: Expr2 not int or char type")
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
        Seq("ArithBio: Expr2 not int or char type")
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

    if (checkExpr(expr1) != BoolType()) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(BoolType()),
        Seq("ArithBio: Expr1 not bool type")
      )
    }

    if (checkExpr(expr2) != BoolType()) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(BoolType()),
        Seq("ArithBio: Expr2 not bool type")
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
    if (checkExpr(expr) != argType) {
      semErr += buildTypeError(
        expr.pos,
        type1,
        Set(retType),
        Seq(s"Unary: argument not $argType")
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
          Seq("Len: argument not array")
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
          Seq(s"Ident: identifier with name ${ident.name} not in scope")
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
    var thisLayer = checkExpr(ident)

    /* for every layer of index, check validity */
    for (i <- indexes) {
      thisLayer = oneArrayElemCheck(thisLayer, i, ident, thisLayer)
    }

    thisLayer
  }

  /* One layer of array elem check */
  def oneArrayElemCheck(t: Type, index: Expr, ident: Ident, idType: Type)(
      implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val indexType = checkExpr(index)
    if (indexType != IntType()) {
      semErr += buildTypeError(
        index.pos,
        indexType,
        Set(IntType()),
        Seq("ArrayElem: index not int type")
      )
      return AnyType()
    }

    /* First argument type should be T[] */
    t match {
      case AnyType()     => return AnyType()
      case ArrayType(t1) => return t1
      case _ => {
        semErr += buildTypeError(
          ident.pos,
          idType,
          Set(ArrayType(AnyType())),
          Seq("ArrayElem: more extract than array nesting")
        )
        return AnyType()
      }
    }
  }

}
