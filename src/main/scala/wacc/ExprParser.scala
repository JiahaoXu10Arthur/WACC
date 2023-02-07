package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.expr.{precedence}
import parsley.combinator.{some}
import Ast.{Expr}
import parsley.expr.{GOps, InfixL, Prefix}
import Lexer.implicitVals._
import parsley.errors.combinator._

object ExprParser {
	lazy val expr: Parsley[Expr] = precedence[Expr](
		//tightest
		Ast.IntLit(Lexer.num),
		Ast.BoolLit(Lexer.bool),
		Ast.CharLit(Lexer.character),
		Ast.StrLit(Lexer.str),
		(Ast.PairLit <# Lexer.pair),
		attempt(Ast.ArrayElem(Ast.Ident(Lexer.ident), some("[" ~> expr <~ "]"))),
		Ast.Ident(Lexer.ident),
		attempt(Ast.IntLit("+" ~> Lexer.num)),
		("(" ~> expr <~ ")")) (

		// unary precedence 0)
		GOps(Prefix)((Ast.Not <# "!"),
								(Ast.Neg <# Lexer.negate),
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
	).label("expression")
		
	def exprParse (input: String): Option[Expr] = {
		expr.parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    	}
	}

}
