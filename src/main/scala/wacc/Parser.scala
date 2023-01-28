package wacc

import ExprParser.{expr}
import parsley.{Parsley, Success, Failure}
import parsley.implicits.character.{charLift, stringLift}
import parsley.combinator.{eof}
import Lexer.{token}
import Ast.Expr

object Parser {
  val program = token("begin") ~> expr <~ token("end") <~ eof
  
  def parse(input: String): Option[Expr] = {
    program.parse(input) match {
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
