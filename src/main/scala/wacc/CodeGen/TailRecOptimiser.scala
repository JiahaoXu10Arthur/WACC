package wacc.CodeGen

import wacc.Ast._

object TailRecOptimiser {
  private final val numInstrsToInspect = 2

  def optimiseFunc(func: Func): Func =
    Func(func.type1, func.ident, func.params, optimiseFuncBody(func.stats, func.ident))(func.pos)

  /* TODO: account for overloaded functions */
  private def optimiseFuncBody(body: Seq[Stat], fname: Ident): List[Stat] = {
    val nonTailBody = body.dropRight(numInstrsToInspect)
    val tailBody = body.takeRight(numInstrsToInspect)
    val mutableBody = nonTailBody.toBuffer

    mutableBody ++= (tailBody match {
      case Seq(stmt, Return(expr: Lvalue)) => {
        stmt match {
          case Assign(expr, Call(fname, _)) => Seq(TailRecurse(fname))
          case Declare(_, expr, Call(fname, _)) => Seq(TailRecurse(fname))
          case _ => tailBody
        }
      }
      case Seq(stmt, ifStmt@If(expr, stat1, stat2)) => Seq(
        stmt,
        If(expr, optimiseFuncBody(stat1, fname), optimiseFuncBody(stat2, fname))(ifStmt.pos)
      )
      case _ => tailBody
    })

    mutableBody.toList
  }
}
