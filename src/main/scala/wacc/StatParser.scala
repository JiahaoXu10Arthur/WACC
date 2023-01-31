package wacc

import parsley.{Parsley, Success, Failure}
import parsley.lift.{lift2, lift3}
import parsley.combinator.{sepBy1}
import Ast.{Stat}
import Lexer.implicitVals._
import ExprParser.{expr}

object StatParser {
  val exit_ = ("exit" ~> expr).map(Ast.Exit(_))
  val print_ = ("print" ~> expr).map(Ast.Print(_))
  val println_ = ("println" ~> expr).map(Ast.Println(_))
  val free_ = ("free" ~> expr).map(Ast.Free(_))
  val read_ = ("read" ~> ValueParser.lvalue).map(Ast.Read(_))
  val ret_ = ("return" ~> expr).map(Ast.Return(_))
  val skip_ = ("skip" #> Ast.Skip())

  lazy val begin_ = ("begin" ~> stmts <~ "end").map(Ast.Begin(_))
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
  val assign_ = lift2[Ast.Lvalue, Ast.Rvalue, Ast.Stat](
                 Ast.Assign(_, _), 
                 ValueParser.lvalue,
                 ("=" ~> ValueParser.rvalue)
               )


  lazy val stmt: Parsley[Stat] = skip_ | exit_ | print_ | println_ | free_ | 
                                 ret_ | if_ | while_ | begin_ | assign_ | read_
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
