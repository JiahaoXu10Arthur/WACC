package wacc

import parsley.{Parsley, Success, Failure}
import parsley.character.char
import parsley.expr.precedence
import Ast.{Expr, UnaryOper, BinaryOper}

import parsley.expr.{Atoms, Ops, InfixL, Prefix}

object Parser {
	lazy val expr: Parsley[Expr] = precedence(

		// tightest
		Atoms(Lexer.num.map(Ast.IntLit(_)),
					Lexer.bool.map(Ast.BoolLit(_)),
					Lexer.char.map(Ast.CharLit(_)),
					Lexer.str.map(Ast.StrLit(_)),
					Lexer.pairLit.map(Ast.PairLit(_)),
					Lexer.ident.map(Ast.Ident(_)), 
					('(' ~> expr.map(Ast.Parens(_)) <~ ')')) :+
		
		// unary
		Ops(Prefix) (('!' #> Ast.Not(_)),
								 ('-' #> Ast.Neg(_)),
								 ("len" #> Ast.Len(_)),
								 ("ord" #> Ast.Ord(_)),
								 ("chr" #> Ast.Chr(_))) :+

		// binary precedence 1
		Ops(InfixL) (('*' #> Ast.Mul(_, _)),
								 ('/' #> Ast.Div(_, _)),
								 ('%' #> Ast.Mod(_, _))) +:

		// binary precedence 2
		Ops(InfixL) (('+' #> Ast.Add(_, _)),
    							('-' #> Ast.Sub(_, _))) +:
		
		// binary precedence 3
		Ops(InfixL) (('>' #> Ast.Gt(_, _)),
    						 (">=" #> Ast.Gte(_, _)),
								 ('<' #> Ast.Lt(_, _)),
								 ("<=" #> Ast.Lte(_, _))) +:

		// binary precedence 4
		Ops(InfixL) (("==" #> Ast.Eq(_, _)),
								("!=" #> Ast.Neq(_, _))) +:

		// binary precedence 5
		Ops(InfixL) (("&&" #> Ast.And(_, _))) +:

		// binary precedence 6
		Ops(InfixL) (("||" #> Ast.Or(_, _)))
		
	)
		

	def exprParse (input: String): Boolean = {
		expr.parse(input) match {
      case Success(x) => true
      case Failure(msg) => false
    }
	}

    
}
