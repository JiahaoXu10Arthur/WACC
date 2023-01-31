package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.expr.{precedence}
import parsley.combinator.{some}
import Ast.{Expr}
import parsley.expr.{SOps, InfixL, Prefix}
import parsley.lift.{lift2}
import Lexer.implicitVals._

object ExprParser {
	lazy val expr: Parsley[Expr] = precedence[Expr](

		//tightest
		Ast.IntLit(Lexer.num),
		Ast.BoolLit(Lexer.bool),
		Ast.CharLit(Lexer.char),
		Ast.StrLit(Lexer.str),
		Ast.PairLit(Lexer.pair),
		attempt (lift2[String, List[Ast.Expr], Ast.ArrayElem] (
			Ast.ArrayElem(_, _),
			(Lexer.ident),
			(some("[" ~> expr <~ "]"))
		)) <|>
		Ast.Ident(Lexer.ident),
		("(" ~> expr <~ ")")) (

		//TODO: fix tokens
		// unary precedence 0)
		SOps(Prefix)((Ast.Not <# "!"),
								(Ast.Neg <# "-"),
								(Ast.Len <# "len"),
								(Ast.Ord <# "ord"),
								(Ast.Chr <# "chr")),
									
		// binary precendence 1
		SOps(InfixL) ((Ast.Mul <# "*"),
								 (Ast.Div <# "/"),
								 (Ast.Mod <# "%")),

		// binary precedence 2
		SOps(InfixL) ((Ast.Add <# "+"),
    						 (Ast.Sub <# "-")),
		
		// binary precedence 3
		SOps(InfixL) ((Ast.Lte <# "<="),
								 (Ast.Lt <# "<"),
    						 (Ast.Gte <# ">="),
								 (Ast.Gt <# ">")),

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
