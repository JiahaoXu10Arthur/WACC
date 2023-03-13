package wacc.CodeGen

import wacc.Ast._

object TailRecOptimiser {
  private final val numInstrsToInspect = 2

  def optimiseFunc(func: Func): Func = {
    val optFunc = Func(func.type1, func.ident, func.params, optimiseFuncBody(func.stats, func.ident))(func.pos)
    optFunc.symb = func.symb
    optFunc
  }

  /* TODO: account for overloaded functions */
  private def optimiseFuncBody(body: Seq[Stat], fname: Ident): List[Stat] = {
    var numInstrsToDrop = 1
    val bodyLast = body.last 
    val newBody: List[Stat] = (bodyLast match {
      case Return(expr : Lvalue) if body.length > 1 => {
        val sndToLast = body(body.length - 2)
        sndToLast match {
          case Assign(id, cstmt@Call(fid, args)) if id == expr && fid == fname => {
            numInstrsToDrop = 2
            val tailRecStmt = TailRecurse(cstmt)

            val expectRet = ident.returnType
            val expectArgType = ident.argType

            val thisRet = call.returnType
            val thisArgType = call.argType
            checkExprType

            sameFunction()

            tailRecStmt.symb = sndToLast.symb
            List(tailRecStmt)
          }
          case Declare(type1, id, cstmt@Call(fid, args)) if expr == id && fid == fname => {
            numInstrsToDrop = 2
            val tailRecStmt = TailRecurse(cstmt)
            tailRecStmt.symb = sndToLast.symb
            List(tailRecStmt)
          }
          case _ => {
            List(bodyLast)
          }
        }
      }
      case ifStmt@If(cond, stat1, stat2) => {
        val newIf = If(cond, optimiseFuncBody(stat1, fname), optimiseFuncBody(stat2, fname))(ifStmt.pos)
        newIf.symb = bodyLast.symb
        List(newIf)
      }
      case _ => List(bodyLast)
    })
    
    val newFuncBody = body.dropRight(numInstrsToDrop).toList ++ newBody
    newFuncBody
  }
}
