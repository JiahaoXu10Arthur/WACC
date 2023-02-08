package wacc

import parsley.{Parsley}
import parsley.combinator.{eof}
import Lexer.{fully}
import Ast.{Program}
import StatParser.{stmts}
import FuncParser.{funcs}
import Lexer.implicitVals._

object Parser {
  // For showing Syntax Error message
  implicit val eb = new SyntaxErrorBuilder

  val program: Parsley[Program] = fully(
    "begin" ~> Program(funcs, stmts) <~ "end" <~ eof
  )

  val parse = (input: String) => program.parse(input)
}
