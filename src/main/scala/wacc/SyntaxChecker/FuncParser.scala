package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt, lookAhead}
import parsley.combinator.{sepBy, many}
import parsley.errors.combinator._
import parsley.errors.patterns.VerifiedErrors

import wacc.Ast._
import Lexer.implicitVals._
import TypeParser.type_

object FuncParser {
  private def bodyEndsWithRet(body: List[Stat]): Boolean = {
    body.last match {
      case Return(_) | Exit(_) => true
      case If(_, s1, s2) =>
        bodyEndsWithRet(s1) &&
        bodyEndsWithRet(s2)
      case Begin(s) => bodyEndsWithRet(s)
      case _        => false
    }
  }
  val param = Param(type_, Ident(Lexer.ident)).label("function parameter")

  val func: Parsley[Func] = (
    Func(
      type_,
      Ident(Lexer.ident),
      ("(" ~> sepBy(param, ",") <~ ")"),
      "is" ~> StatParser.stmts <~ "end"
   )
  ).guardAgainst {
    case Func(_, id, _, body) if !bodyEndsWithRet(body) =>
      Seq(
        s"Function `${id.name}` is missing a return on all exit paths!",
        "All functions must end with a return or exit statement."
      )
  }.label("function")

  /* Error widget definitions */
  private val _funcMissingTypeCheck: Parsley[Nothing] = {
    attempt(Ident(Lexer.ident) <~ "(").verifiedFail(n =>
      Seq(s"Function definition for `${n.name}` is missing a return type!")
    )
  }
  private val _funcStartCheck = {
    attempt(lookAhead(type_ ~> Lexer.ident ~> "("))
  }

  val funcs: Parsley[List[Func]] = many(
    _funcMissingTypeCheck | _funcStartCheck ~> func
  )

  def funcParse(input: String): Option[Func] = {
    func.parse(input) match {
      case Success(x) => {
        Some(x)
      }
      case Failure(msg) => {
        None
      }
    }
  }
}
