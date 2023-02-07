package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr
import ExprSemantic._
import SymbolObject._
import SymbolObjectType._

object ValueSemantic {
  def checkLvalue(lvalue: Lvalue)(implicit st: SymbolTable): Type = {
    lvalue match {
      case lvalue: Expr => checkExpr(lvalue)
      case lvalue: Rvalue => checkRvalue(lvalue)
    }
  }

  def checkRvalue(rvalue: Rvalue)(implicit st: SymbolTable): Type = { 
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
  def newPairCheck(expr1: Expr, expr2: Expr)(implicit st: SymbolTable): Type = { 
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)
    PairType(type1, type2)
  }

  /* Arg1: Ident -> Refer to FuncObj in st
     FuncObj(returnType, args, argc, st) 
     Arg2: args -> type match to FuncObj(args)
     Return: FuncObj(returnType) */
  def callCheck(ident: Ident, args: List[Expr])(implicit st: SymbolTable): Type = { 
    var funcObj: FuncObj = null
    /* Find funcObj */
    st.lookUpAll(ident.name, FunctionType()) match {
      case Some(symObj) =>  { 
          symObj match {
          case symObj: FuncObj => funcObj = symObj
          case _ => {
            semanticErr("Call: fst arg not funcObj")
            AnyType()
          }
        }
      }
      case None => {
        semanticErr(s"Call: function name ${ident.name} not in symbol table")
        AnyType()
      }
    }

    /* check number of parameters */
    if (args.length != funcObj.argc) {
      semanticErr("Call: wrong argument number")
    }

    /* check every parameter's type */
    for (i <- 0 until args.length) {
      if (!equalType(checkExpr(args(i)), funcObj.args(i).getType())) {
        semanticErr("Call: argument type does not match")
      }
    }

    /* Return type */
    funcObj.returnType
  }

  /* Arg1: fst/snd
     Arg2: PairType(T1, T2)
     Return: if fst -> T1; if snd -> T2 */
  def pairElemCheck(index: String, lvalue: Lvalue)(implicit st: SymbolTable): Type = { 
    var returnType: Type = null

    /* Lvalue should be a pair */
    val lType = checkLvalue(lvalue)
    lType match {
      case PairType(t1, t2) => index match {
        case "fst" => returnType = t1
        case "snd" => returnType = t2
      }
      case _ => {
        semanticErr("Pair elem: not Pair Type")
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
  def arrayLitCheck(values: List[Expr])(implicit st: SymbolTable): Type = { 
    /* check empty */
    if (values.length == 0) {
      return ArrayType(AnyType())
    }
    
    /* check every parameter's type */
    for (i <- 0 until values.length - 1) {
      // checkValueRef(args(i), st)
      if (checkExpr(values(i)) != checkExpr(values(i + 1))) {
        semanticErr("ArrayLit: Array literals are not of the same type")
        return ArrayType(AnyType())
      }
    }
    ArrayType(checkExpr(values(0)))
  }

}
