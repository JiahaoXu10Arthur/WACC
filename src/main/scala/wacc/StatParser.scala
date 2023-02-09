package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.{sepBy1}
import Ast._
import Lexer.implicitVals._
import ExprParser.{expr}
import parsley.errors.combinator._

object StatParser {
  val exit_ = "exit" ~> Exit(expr).label("exit statement")
  val print_ = "print" ~> Print(expr).label("print statement")
  val println_ = "println" ~> Println(expr).label("println statement")
  val free_ = "free" ~> Free(expr).label("free statement")
  val read_ = "read" ~> Read(ValueParser.lvalue).label("read statement")
  val ret_ = "return" ~> Return(expr).label("return statement")
  val skip_ = Skip <# "skip".label("skip statement")

  lazy val begin_ =
    ("begin" ~> Begin(stmts) <~ "end").label("begin statement")
  lazy val if_ = If(
    "if" ~> expr,
    "then" ~> stmts,
    "else" ~> stmts <~ "fi"
  ).label("if statement")
  lazy val while_ =
    While(
      "while" ~> expr,
      "do" ~> stmts <~ "done"
    ).label("while statement")
  val assign_ = Assign(ValueParser.lvalue, "=" ~> ValueParser.rvalue)
    .label("assign statement")
  val declare_ = Declare(
    TypeParser.type_,
    Ident(Lexer.ident),
    "=" ~> ValueParser.rvalue
  ).label("declare statement")

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
