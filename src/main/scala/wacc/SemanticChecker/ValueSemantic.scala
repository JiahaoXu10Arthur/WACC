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
      case lvalue: StructElem => structElemCheck(lvalue.ident, lvalue.field)
    }
  }

  def checkRvalue(
      rvalue: Rvalue,
      targetType: Type
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    rvalue match {
      case NewPair(e1, e2)               => newPairCheck(e1, e2)
      case callValue @ Call(ident, args) => {
        // Assign return type for callValue AST node
        callValue.returnType = targetType
        callCheck(ident, args, targetType)
      }
      case PairElem(index, lvalue)       => pairElemCheck(index, lvalue)
      case ArrayLit(values)              => arrayLitCheck(values)
      case rvalue: Expr                  => checkExpr(rvalue)
      case StructLit(fields)             => structLitCheck(fields, targetType)
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
  def callCheck(ident: FuncIdent, args: List[Expr], targetType: Type)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val expectedArgs = args.map(checkExprType(_))
    var funcName: String = ""

    val (funcObj, findFunc, funcSymb) = ident match {
      case Ident(name) => 
        funcName = name
        callFuncCheck(name, targetType, expectedArgs)
      case ClassFuncCall(ident, func) => 
        funcName = ident.getIdent.name + "." + func.name
        callClassFuncCheck(ident, func, targetType, expectedArgs)
    }

    /* If no function found, error */
    if (!findFunc) {
      val similarFuncs = st.lookUpAllSimilarFunc(ident.getIdent.name)
      semErr += buildFunctionScopeError(
          ident.pos,
          funcName,
          similarFuncs,
          Seq(s"Function ${funcName} with " +
              s"argument type ${expectedArgs} and " +
              s"return type ${targetType} has not been defined")
        )
        /* Add fake function to the scope to avoid further error */
        funcSymb.add(
          ident.getIdent.name,
          FunctionType(),
          new FuncObj(AnyType(), List(), 0, st, ident.pos)
        )
      return AnyType()
    }

    /* Return type */
    funcObj.returnType
  }

  private def callFuncCheck(funcName: String, 
                            targetType: Type, 
                            expectedArgs: List[Type])(implicit
      st: SymbolTable): (FuncObj, Boolean, SymbolTable) = {

    /* Search funcObj in all scope*/
    st.lookUpAllFunc(funcName) match {
      case Some(symObj) => {
        val funcObjOpt = st.getOverloadFuncObj(funcName, targetType, expectedArgs)
        // If can find function with same return type and arguments
        funcObjOpt match {
          case Some(obj) => return (obj, true, st)
          case None =>
        }

      }
      case _ =>
    }

    (null, false, st)
  }

  private def callClassFuncCheck(classIdent: Ident, 
                                 func: Ident,
                                 targetType: Type, 
                                 expectedArgs: List[Type])(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]): (FuncObj, Boolean, SymbolTable) = {

    val classType = checkExpr(classIdent)
    classType match {
      case AnyType() => 
      case ClassType(className) => {
        // Check class is defined
        st.lookUpAll(className.name, ClassObjType()) match {
          // recursive call in class's symbol table
          case Some(obj: ClassObj) => return callFuncCheck(className.name, targetType, expectedArgs)(obj.symTable)
          case _ => 
				}
      }
      case _ => {
        semErr += buildScopeError(
              classIdent.pos,
              classIdent.name,
              st.lookUpAllSimilar(classIdent.name, ClassObjType()),
              Seq(s"Class ${classIdent.name} has not been declared in this scope")
            )
      }
    }

    (null, false, st)
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
      case AnyType() => returnType = AnyType()
      case _ => {
        semErr += buildTypeError(
          lvalue.pos,
          lType,
          Set(PairType(AnyType(), AnyType())),
          Seq("Keywords fst and snd should be applied on pairs")
        )
        returnType = AnyType()
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

  def structLitCheck(
      fields: List[Expr],
      targetType: Type
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    if (fields.isEmpty) {
      return AnyType()
    }

    val fieldsType = fields.map(checkExpr(_))

    targetType match {
      case StructType(ident) => {
        val structObj = st.lookUpAll(ident.name, StructObjType())
        structObj match {
          case Some(structObj: StructObj) => {
            val structFields = structObj.fields
            // struct size not match
            if (fields.size != structFields.size) {
              semErr += buildArgNumError(
                fields(fields.size - 1).pos,
                fields.size,
                structFields.size,
                Seq("Number of struct/class fields does not match defined type")
              )
              return AnyType()
            }

            var i = 0
            // Check every field type suits target struct type
            while (i < fieldsType.length) {
              if (!equalType(fieldsType(i), structFields(i)._2.getType())) {
                semErr += buildTypeError(
                  fields(i).pos,
                  fieldsType(i),
                  Set(structFields(i)._2.getType()),
                  Seq("Struct/Class field type does not match defined type")
                )
                return AnyType()
              }

              i += 1
            }

          }
          case _ => return AnyType()
        }
      }
      case ClassType(ident) => {
        val classObj = st.lookUpAll(ident.name, ClassObjType())
        classObj match {
          case Some(classObj: ClassObj) => structLitCheck(fields, StructType(ident))(classObj.symTable, semErr)
          case _ => return AnyType()
        }
      }
      case _ => {
        return AnyType()
      }
    }

    // Check every field suits target struct type
    targetType
  }

}

