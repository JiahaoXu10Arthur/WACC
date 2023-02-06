package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.attempt
import parsley.combinator.{sepBy, many}
import parsley.errors.combinator.ErrorMethods
import Lexer.implicitVals._
import Ast._
import TypeParser.type_

object FuncParser {
  private def bodyEndsWithRet (body: List[Stat]): Boolean = {
    body.last match {
      case Return(_) | Exit(_) => true
      case If(_, s1, s2) => bodyEndsWithRet(s1) &&
                            bodyEndsWithRet(s2)
      case Begin(s) => bodyEndsWithRet(s)
      case _ => false
    }
  }
  val param = Param(type_, Ast.Ident(Lexer.ident))

  val func: Parsley[Func] = attempt(Func(
    type_,
    Ast.Ident(Lexer.ident),
    "(" ~> sepBy(param, ",") <~ ")",
    "is" ~> StatParser.stmts <~ "end"
  )).guardAgainst {
    case Func(_, _, _, body) if !bodyEndsWithRet(body) =>
      Seq("Function body does not end with a return statement")
  }

  val funcs: Parsley[List[Func]] = many(func)

  def funcParse (input: String): Option[Func] = {
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
