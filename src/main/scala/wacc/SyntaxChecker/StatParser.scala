package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.combinator.{sepBy1}
import parsley.errors.patterns.{VerifiedErrors}
import parsley.errors.combinator._

import wacc.Ast._

import Lexer.implicitVals._
import ExprParser.{expr}
import TypeParser.type_
import wacc.Ast

object StatParser {
  val exit_ = "exit" ~> Exit(expr)
  val print_ = "print" ~> Print(expr)
  val println_ = "println" ~> Println(expr)
  val free_ = "free" ~> Free(expr)
  val read_ = "read" ~> Read(ValueParser.lvalue)
  val ret_ = "return" ~> Return(expr)
  val skip_ = Skip <# "skip"

  lazy val begin_ =
    "begin" ~> Begin(stmts) <~ "end"

  // Explain for the structure of if and while statements
  lazy val if_ = 
    If(
      "if" ~> expr,
      "then" ~> stmts,
      "else".explain(
        "all if statements must have an else clause"
      ) ~> stmts <~ "fi".explain("unclosed if statement")
    )
    .label("if statement")
  lazy val while_ =
    While(
        "while" ~> expr,
        "do".explain(
          "all while statements must have an do clause"
        ) ~> stmts <~ "done".explain("unclosed while loop")
      )
      .label("while statement")

  val assign_ = Assign(ValueParser.lvalue, "=" ~> ValueParser.rvalue)

  // lable declartion "=" as type
  // distinguish from assignment "="
  val declare_ = Declare(
      TypeParser.type_.label("type"),
      Ident(Lexer.ident),
      "=" ~> ValueParser.rvalue
    )

  /* Error widget definitions */
  private val _funcDefCheck = {
    attempt((type_ ~> Lexer.ident <~ "("))
      .verifiedFail(n => Seq(
        s"Unexpected function definition for `${n}`!", 
        "all functions must be declared at the top of the main block"
        )
      )
  }
  lazy val stmt: Parsley[Stat] = _funcDefCheck | skip_ | exit_ | print_ |
    println_ | free_ | ret_ | if_ | while_ | begin_ | declare_ | assign_ | read_
  lazy val stmts: Parsley[List[Stat]] = sepBy1(stmt, ";")

  def statParse(input: String): Option[List[Stat]] = {
    stmts.parse(input) match {
      case Success(x)   => Some(x)
      case Failure(msg) => None
    }
  }
}
