package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr
import ExprSemantic._
import SymbolObject._
import SymbolObjectType._
import Errors._
import SemanticErrorBuilder._
import scala.collection.mutable.ListBuffer

object ValueSemantic {
  def checkLvalue(lvalue: Lvalue)
                 (implicit st: SymbolTable, 
                           semErr: ListBuffer[WACCError]): Type = {
    lvalue match {
      case lvalue: Expr => checkExpr(lvalue)
      case lvalue: Rvalue => checkRvalue(lvalue)
    }
  }

  def checkRvalue(rvalue: Rvalue)
                 (implicit st: SymbolTable, 
                           semErr: ListBuffer[WACCError]): Type = { 
    rvalue match {
      case NewPair(e1, e2) => newPairCheck(e1, e2)
      case Call(ident, args) => callCheck(ident, args)
      case PairElem(index, lvalue) => pairElemCheck(index, lvalue)
      case ArrayLit(values) => arrayLitCheck(values)
      case rvalue: Expr => checkExpr(rvalue)
    }
  }

  /* Arg1: PairElementType
     Arg2: PairElementType
     Return: PairType(Arg1Type, Arg2Type) */
  def newPairCheck(expr1: Expr, expr2: Expr)
                  (implicit st: SymbolTable, 
                            semErr: ListBuffer[WACCError]): Type = { 
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)
    PairType(type1, type2)
  }

  /* Arg1: Ident -> Refer to FuncObj in st
     FuncObj(returnType, args, argc, st) 
     Arg2: args -> type match to FuncObj(args)
     Return: FuncObj(returnType) */
  def callCheck(ident: Ident, args: List[Expr])
               (implicit st: SymbolTable, 
                         semErr: ListBuffer[WACCError]): Type = { 
    var funcObj: FuncObj = null
    /* Find funcObj */
    st.lookUpAll(ident.name, FunctionType()) match {
      case Some(symObj: FuncObj) => funcObj = symObj
      case _ => {
        semErr += buildScopeError(None, ident.pos, ident.name, st.lookUpAllSimilar(ident.name, FunctionType()), 
                                  Seq(s"Call: function with name ${ident.name} not in scope"), "")
        st.add(ident.name, FunctionType(), new FuncObj(AnyType(), List(), 0, st, ident.pos))
        return AnyType()
      }
    }

    val lengthArgs = args.length
    /* check number of parameters */
    if (lengthArgs != funcObj.argc) {
      semErr += buildArgNumError(None, args(lengthArgs - 1).pos,
                       lengthArgs, funcObj.argc, 
                       Seq("Call: wrong argument number"), "")
    }

    /* check every parameter's type */
    for (i <- 0 until lengthArgs.min(funcObj.argc)) {
      val type1 = funcObj.args(i).getType()
      val type2 = checkExpr(args(i))
      if (!equalType(type1, type2)) {
        semErr += buildTypeError(None, args(i).pos, type2, Set(type1), 
                               Seq("Call: argument type does not match"), "")
      }
    }

    /* Return type */
    funcObj.returnType
  }

  /* Arg1: fst/snd
     Arg2: PairType(T1, T2)
     Return: if fst -> T1; if snd -> T2 */
  def pairElemCheck(index: String, lvalue: Lvalue)
                   (implicit st: SymbolTable, 
                             semErr: ListBuffer[WACCError]): Type = { 
    var returnType: Type = null

    /* Lvalue should be a pair */
    val lType = checkLvalue(lvalue)
    lType match {
      case PairType(t1, t2) => index match {
        case "fst" => returnType = t1
        case "snd" => returnType = t2
      }
      case _ => {
        semErr += buildTypeError(None, lvalue.pos, lType, Set(PairType(AnyType(), AnyType())), 
                               Seq("Pair elem: not Pair Type"), "")
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
  def arrayLitCheck(values: List[Expr])
                   (implicit st: SymbolTable, 
                             semErr: ListBuffer[WACCError]): Type = { 
    /* check empty */
    if (values.length == 0) {
      return ArrayType(AnyType())
    }
    
    /* check every parameter's type */
    for (i <- 0 until values.length - 1) {
      // checkValueRef(args(i), st)
      val type1 = checkExpr(values(i))
      val type2 = checkExpr(values(i + 1))

      if (type1 != type2) {
        semErr += buildTypeError(None, values(i + 1).pos, type2, Set(type1), 
                               Seq("ArrayLit: Array literals are not of the same type"), "")
        return ArrayType(AnyType())
      }
    }
    ArrayType(checkExpr(values(0)))
  }

}
