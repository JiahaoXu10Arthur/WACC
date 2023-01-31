package wacc

import parsley.{Parsley}
//import parsley.combinator.{eof}
import Lexer.{fully}
import Ast.{Stat}
import StatParser.{stmts}
import Lexer.implicitVals._

object Parser {
  val program: Parsley[List[Stat]] = fully("begin" ~> stmts <~ "end")
  
  val parse = (input: String) => program.parse(input)
}
