package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer

import wacc.Ast._
import wacc.Error.SemanticErrorBuilder._
import wacc.Error.Errors._

import SemanticTypes._
import ExprSemantic._
import SymbolObject._
import SymbolObjectType._

object ValueSemantic {
  def checkLvalue(
      lvalue: Lvalue
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    lvalue match {
      case lvalue: Ident     => identCheck(lvalue)
      case lvalue: ArrayElem => arrayElemCheck(lvalue.ident, lvalue.index)
      case lvalue: PairElem  => pairElemCheck(lvalue.index, lvalue.lvalue)
    }
  }

  def checkRvalue(
      rvalue: Rvalue,
      targetType: Type
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    rvalue match {
      case NewPair(e1, e2)         => newPairCheck(e1, e2)
      case Call(ident, args)       => callCheck(ident, args, targetType)
      case PairElem(index, lvalue) => pairElemCheck(index, lvalue)
      case ArrayLit(values)        => arrayLitCheck(values)
      case rvalue: Expr            => checkExpr(rvalue)
    }
  }

  /* Arg1: PairElementType
     Arg2: PairElementType
     Return: PairType(Arg1Type, Arg2Type) */
  def newPairCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)
    PairType(type1, type2)
  }

  /* Arg1: Ident -> Refer to FuncObj in st
     FuncObj(returnType, args, argc, st)
     Arg2: args -> type match to FuncObj(args)
     Return: FuncObj(returnType) */
  def callCheck(ident: Ident, args: List[Expr], targetType: Type)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    var funcObj: FuncObj = null
    var findFunc = true
    /* Search funcObj in all scope*/
    st.lookUpAllFunc(ident.name) match {
      case Some(symObj) => {
        val expectedArgs = args.map(checkExprType(_))
        val funcObjOpt = st.getOverloadFuncObj(ident.name, targetType, expectedArgs)
        funcObjOpt match {
          case Some(obj) => funcObj = obj
          case None => findFunc = false
        }

      }
      case _ => 
    }

    /* If no function found, error */
    if (!findFunc) {
      semErr += buildScopeError(
          ident.pos,
          ident.name,
          st.lookUpAllSimilar(ident.name, FunctionType()),
          Seq(s"Function ${ident.name} has not been defined")
        )
        /* Add fake function to the scope to avoid further error */
        st.add(
          ident.name,
          FunctionType(),
          new FuncObj(AnyType(), List(), 0, st, ident.pos)
        )
      return AnyType()
    }

    /* Return type */
    funcObj.returnType
  }

  /* Arg1: fst/snd
     Arg2: PairType(T1, T2)
     Return: if fst -> T1; if snd -> T2 */
  def pairElemCheck(index: String, lvalue: Lvalue)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    var returnType: Type = null

    /* Lvalue should be a pair */
    val lType = checkLvalue(lvalue)
    lType match {
      case PairType(t1, t2) =>
        index match {
          case "fst" => returnType = t1
          case "snd" => returnType = t2
        }
      case _ => {
        semErr += buildTypeError(
          lvalue.pos,
          lType,
          Set(PairType(AnyType(), AnyType())),
          Seq("Keywords fst and snd should be applied on pairs")
        )
        AnyType()
      }
    }

    returnType
  }

  /* Arg: All elements in List same type T
     Return: ArrayType(T)
     OR
     Arg: [] -> empty list
     Return: ArrayType(AnyType) */
  def arrayLitCheck(
      values: List[Expr]
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    /* check empty */
    if (values.length == 0) {
      return ArrayType(AnyType())
    }

    /* check every element's type is the same */
    for (i <- 0 until values.length - 1) {
      val type1 = checkExpr(values(i))
      val type2 = checkExpr(values(i + 1))

      if (!equalType(type1, type2)) {
        semErr += buildTypeError(
          values(i + 1).pos,
          type2,
          Set(type1),
          Seq("All array elements should have the same type")
        )
        ArrayType(AnyType())
      }
    }

    ArrayType(checkExpr(values(0)))
  }

}
