package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.character.char
import parsley.expr.precedence
import parsley.implicits.character.{charLift, stringLift}
import Ast.{Expr}
import parsley.expr.{Atoms, SOps, Ops, InfixL, Prefix}
import parsley.debug._

object Parser {
	lazy val expr: Parsley[Expr] = precedence[Expr](

		//tightest
		Ast.IntLit(Lexer.num),
		Ast.BoolLit(Lexer.bool),
		Ast.CharLit(Lexer.char),
		Ast.StrLit(Lexer.str),
		Ast.PairLit(Lexer.pair),
		Ast.Ident(Lexer.ident),
		('(' ~> expr <~ ')')) (

		// unary precedence 0)
		Ops(Prefix)((Ast.Not <# '!'),
								(Ast.Neg <# '-'),
								(Ast.Len <# "len"),
								(Ast.Ord <# "ord"),
								(Ast.Chr <# "chr")),
									
		// binary precendence 1
		Ops(InfixL) ((Ast.Mul <# '*'),
								 (Ast.Div <# '/'),
								 (Ast.Mod <# '%')),

		// binary precedence 2
		Ops(InfixL) ((Ast.Add <# '+'),
    						 (Ast.Sub <# '-')),
		
		// binary precedence 3
		Ops(InfixL) ((attempt(Ast.Lte <# "<=")) <|> (Ast.Lt <# "<"),
    						 (attempt(Ast.Gte <# ">=")) <|> (Ast.Gt <# ">")),

		// binary precedence 4
		Ops(InfixL) ((Ast.Eq <# "=="),
								 (Ast.Neq <# "!=")),

		// binary precedence 5
		Ops(InfixL) ((Ast.And <# "&&")),

		// binary precedence 6
		Ops(InfixL) ((Ast.Or <# "||"))
	)
		
	def exprParse (input: String): Option[Expr] = {
		expr.debug("debug starts").parse(input) match {
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
