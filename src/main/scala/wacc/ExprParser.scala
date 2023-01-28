package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.expr.precedence
import parsley.implicits.character.{charLift, stringLift}
import Ast.{Expr}
import parsley.expr.{SOps, InfixL, Prefix}

object ExprParser {
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
		SOps(Prefix)((Ast.Not <# '!'),
								(Ast.Neg <# '-'),
								(Ast.Len <# "len"),
								(Ast.Ord <# "ord"),
								(Ast.Chr <# "chr")),
									
		// binary precendence 1
		SOps(InfixL) ((Ast.Mul <# '*'),
								 (Ast.Div <# '/'),
								 (Ast.Mod <# '%')),

		// binary precedence 2
		SOps(InfixL) ((Ast.Add <# '+'),
    						 (Ast.Sub <# '-')),
		
		// binary precedence 3
		SOps(InfixL) ((attempt(Ast.Lte <# "<=")) <|> (Ast.Lt <# "<"),
    						 (attempt(Ast.Gte <# ">=")) <|> (Ast.Gt <# ">")),

		// binary precedence 4
		SOps(InfixL) ((Ast.Eq <# "=="),
								 (Ast.Neq <# "!=")),

		// binary precedence 5
		SOps(InfixL) ((Ast.And <# "&&")),

		// binary precedence 6
		SOps(InfixL) ((Ast.Or <# "||"))
	)
		
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