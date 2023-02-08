package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.{sepBy1}
import Ast.{Stat}
import Lexer.implicitVals._
import ExprParser.{expr}
import parsley.errors.combinator._

object StatParser {
  val exit_ = "exit" ~> Ast.Exit(expr).label("exit statement")
  val print_ = "print" ~> Ast.Print(expr).label("print statement")
  val println_ = "println" ~> Ast.Println(expr).label("println statement")
  val free_ = "free" ~> Ast.Free(expr).label("free statement")
  val read_ = "read" ~> Ast.Read(ValueParser.lvalue).label("read statement")
  val ret_ = "return" ~> Ast.Return(expr).label("return statement")
  val skip_ = Ast.Skip <# "skip".label("skip statement")

  lazy val begin_ =
    "begin" ~> Ast.Begin(stmts) <~ "end".label("begin statement")
  lazy val if_ = Ast
    .If("if" ~> expr, "then" ~> stmts, "else" ~> stmts <~ "fi")
    .label("if statement")
  lazy val while_ =
    Ast.While("while" ~> expr, "do" ~> stmts <~ "done").label("while statement")
  val assign_ = Ast
    .Assign(ValueParser.lvalue, "=" ~> ValueParser.rvalue)
    .label("assign statement")
  val declare_ = Ast
    .Declare(
      TypeParser.type_,
      Ast.Ident(Lexer.ident),
      "=" ~> ValueParser.rvalue
    )
    .label("declare statement")

  lazy val stmt: Parsley[Stat] = skip_ | exit_ | print_ | println_ | free_ |
    ret_ | if_ | while_ | begin_ | declare_ |
    assign_ | read_
  lazy val stmts: Parsley[List[Stat]] = sepBy1(stmt, ";")

  def statParse(input: String): Option[List[Stat]] = {
    stmts.parse(input) match {
      case Success(x)   => Some(x)
      case Failure(msg) => None
    }
  }
}
