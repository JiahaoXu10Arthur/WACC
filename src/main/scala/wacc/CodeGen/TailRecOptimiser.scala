package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes._

object TailRecOptimiser {
  final private val numInstrsToInspect = 2

  def optimiseFunc(func: Func): Func = {
    implicit val st = func.symb
    val optFunc     = Func(func.type1, func.ident, func.params, optimiseFuncBody(func.stats, func))(func.pos)
    optFunc.symb = func.symb
    optFunc
  }

  private def optimiseFuncBody(body: Seq[Stat], func: Func)(implicit st: SymbolTable): List[Stat] = {
    var numInstrsToDrop = 1
    val bodyLast        = body.last

    val newBody: List[Stat] = bodyLast match {
      case Return(expr: Lvalue) if body.length > 1 =>
        val sndToLast   = body(body.length - 2)
        val tailRecCall = checkRecursiveCall(sndToLast, expr, func)
        tailRecCall match {
          case Some(cstmt) =>
            numInstrsToDrop = 2
            val tailRecStmt = TailRecurse(cstmt)
            tailRecStmt.symb = sndToLast.symb
            List(tailRecStmt)
          case None => List(bodyLast)
        }
      case ifStmt @ If(cond, stat1, stat2) =>
        val newIf = If(cond, optimiseFuncBody(stat1, func), optimiseFuncBody(stat2, func))(ifStmt.pos)
        newIf.symb = bodyLast.symb
        List(newIf)
      case _ => List(bodyLast)
    }

    val newFuncBody = body.dropRight(numInstrsToDrop).toList ++ newBody
    newFuncBody
  }

  private def checkRecursiveCall(stat: Stat, returnExpr: Expr, func: Func)(implicit st: SymbolTable): Option[Call] = {
    val funcRetType    = convertType(func.type1)
    val funcParamTypes = func.params.map(p => convertType(p.paramType))
    stat match {
      case Assign(expr: Expr, cstmt @ Call(fid, args)) =>
        val retType  = checkLvalueType(expr)
        val argTypes = args.map(arg => checkExprType(arg))
        if (
          expr == returnExpr && fid == func.ident &&
          sameFunction(funcRetType, retType, funcParamTypes, argTypes)
        ) {
          Some(cstmt)
        } else {
          None
        }
      case Declare(type1, id, cstmt @ Call(fid, args)) =>
        val retType  = convertType(type1)
        val argTypes = args.map(arg => checkExprType(arg))
        if (
          id == returnExpr && fid == func.ident &&
          sameFunction(funcRetType, retType, funcParamTypes, argTypes)
        ) {
          Some(cstmt)
        } else {
          None
        }
      case _ => None
    }
  }
}
