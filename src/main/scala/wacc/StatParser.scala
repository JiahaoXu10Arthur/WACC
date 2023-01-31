package wacc

import parsley.{Parsley, Success, Failure}
import parsley.lift.{lift2, lift3}
import parsley.combinator.{sepBy1}
import Ast.{Stat}
import Lexer.implicitVals._
import ExprParser.{expr}

object StatParser {
  val exit = ("exit" ~> expr).map(Ast.Exit(_))
  val print_ = ("print" ~> expr).map(Ast.Print(_))
  val println_ = ("println" ~> expr).map(Ast.Println(_))
  val free = ("free" ~> expr).map(Ast.Free(_))
  val ret = ("return" ~> expr).map(Ast.Return(_))
  val skip_ = ("skip" #> Ast.Skip())

  lazy val begin = ("begin" ~> stmts <~ "end").map(Ast.Begin(_))
  lazy val if_ = lift3[Ast.Expr, List[Ast.Stat], List[Ast.Stat], Ast.Stat](
                   Ast.If(_, _, _), 
                   ("if" ~> expr), 
                   ("then" ~> stmts), 
                   ("else" ~> stmts <~ "fi")
                 )
  lazy val while_ = lift2[Ast.Expr, List[Ast.Stat], Ast.Stat](
                      Ast.While(_, _), 
                      ("while" ~> expr),
                      ("do" ~> stmts <~ "done")
                    )


  lazy val stmt: Parsley[Stat] = skip_ | exit | print_ | println_ | free | ret | 
                                 if_ | while_ | begin
  lazy val stmts: Parsley[List[Stat]] = sepBy1(stmt, ";")

  def statParse(input: String): Option[List[Stat]] = {
    stmts.parse(input) match {
      case Success(x) => {
        println(x)
        Some(x)
      }
      case Failure(msg) => {
        println(msg)
        None
      }
    }
  }
}
