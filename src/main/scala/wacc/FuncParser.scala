package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.attempt
import parsley.combinator.{sepBy, many}
import Lexer.implicitVals._
import Ast._
import TypeParser.type_

object FuncParser {
  val param = Param(type_, Ast.Ident(Lexer.ident))
  val func: Parsley[Func] = Func(
    type_,
    Ast.Ident(Lexer.ident),
    "(" ~> sepBy(param, ",") <~ ")",
    "is" ~> StatParser.stmts <~ "end"
  )
  val funcs: Parsley[List[Func]] = many(attempt(func))

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
