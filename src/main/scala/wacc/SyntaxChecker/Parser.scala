package wacc.SyntaxChecker

import parsley.{Parsley}
import parsley.combinator.{eof}
import Lexer.{fully}

import wacc.Ast.{Program}
import wacc.Error.SyntaxErrorBuilder

import StatParser.{stmts}
import FuncParser.{funcs}
import StructParser.{structs}
import Lexer.implicitVals._

object Parser {
  // For showing Syntax Error message
  implicit val eb = new SyntaxErrorBuilder

  val program: Parsley[Program] = fully(
    "begin" ~> Program(structs ,funcs, stmts) <~ "end" <~ eof
  )

  val parse = (input: String) => program.parse(input)
}
