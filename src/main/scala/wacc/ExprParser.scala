package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt, pure}
import parsley.expr.{precedence}
import parsley.combinator.{some}
import Ast.{Expr}
import parsley.expr.{GOps, InfixL, Prefix}
import Lexer.implicitVals._
import parsley.debug._

object ExprParser {
	lazy val expr: Parsley[Expr] = precedence[Expr](
		//tightest
		Ast.IntLit(Lexer.num),
		Ast.BoolLit(Lexer.bool),
		Ast.CharLit(Lexer.char),
		Ast.StrLit(Lexer.str),
		Ast.PairLit(Lexer.pair),
		attempt(Ast.ArrayElem(Ast.Ident(Lexer.ident), some("[" ~> expr <~ "]"))),
		Ast.Ident(Lexer.ident),
		("(" ~> expr <~ ")")) (

		// unary precedence 0)
		GOps(Prefix)((Ast.Not <# "!"),
								(Ast.Neg <# "-"),
								(Ast.Len <# "len"),
								(Ast.Ord <# "ord"),
								(Ast.Chr <# "chr")),
									
		// binary precendence 1
		GOps(InfixL) ((Ast.Mul <# "*"),
								 (Ast.Div <# "/"),
								 (Ast.Mod <# "%")),

		// binary precedence 2
		GOps(InfixL) ((Ast.Add <# "+"),
    						 (Ast.Sub <# "-")),
		
		// binary precedence 3
		GOps(InfixL) ((Ast.Lte <# "<="),
								 (Ast.Lt <# "<"),
    						 (Ast.Gte <# ">="),
								 (Ast.Gt <# ">")),

		// binary precedence 4
		GOps(InfixL) ((Ast.Eq <# "=="),
								 (Ast.Neq <# "!=")),

		// binary precedence 5
		GOps(InfixL) ((Ast.And <# "&&")),

		// binary precedence 6
		GOps(InfixL) ((Ast.Or <# "||"))
	)
		
	def exprParse (input: String): Option[Expr] = {
		println("input: " + input)
		expr.debug("sds").parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}

}
