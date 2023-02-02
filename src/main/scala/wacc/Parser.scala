package wacc

import parsley.{Parsley}
//import parsley.combinator.{eof}
import Lexer.{fully}
import Ast.{Program}
import StatParser.{stmts}
import FuncParser.{funcs}
import Lexer.implicitVals._

object Parser {
  val program: Parsley[Program] 
    = fully("begin" ~> Program(funcs, stmts)<~ "end")
  
  val parse = (input: String) => program.parse(input)
}
