package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.{sepBy1}
import Ast._
import Lexer.implicitVals._
import ExprParser.{expr}
import parsley.errors.combinator._

object StatParser {
  val exit_ = "exit" ~> Exit(expr)
  val print_ = "print" ~> Print(expr)
  val println_ = "println" ~> Println(expr)
  val free_ = "free" ~> Free(expr)
  val read_ = "read" ~> Read(ValueParser.lvalue)
  val ret_ = "return" ~> Return(expr)
  val skip_ = Skip <# "skip"

  lazy val begin_ =
    "begin" ~> Ast.Begin(stmts) <~ "end"

  // Explain for the structure of if and while statements  
  lazy val if_ = Ast
    .If("if" ~> expr, "then" ~> stmts, "else".explain("all if statements must have an else clause") ~> stmts <~ "fi".explain("unclosed if statement"))
    .label("if statement")
  lazy val while_ =
    Ast.While("while" ~> expr, "do".explain("all while statements must have an do clause") ~> stmts <~ "done".explain("unclosed while loop")).label("while statement")
  
  // hide assignment "="
  val assign_ = Ast
    .Assign(ValueParser.lvalue, "=" ~> ValueParser.rvalue).hide

  // lable declartion "=" as type
  // distinguish from assignment "="
  val declare_ = Ast
    .Declare(
      TypeParser.type_.label("type"),
      Ast.Ident(Lexer.ident),
      "=" ~> ValueParser.rvalue
    )

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
