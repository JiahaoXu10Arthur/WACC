package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.expr.{precedence}
import parsley.combinator.{some}
import Ast.{Expr}
import parsley.expr.{GOps, InfixL, Prefix}
import Lexer.implicitVals._
import parsley.errors.combinator._
import parsley.errors.patterns.VerifiedErrors

object ExprParser {
  private val _funcCallCheck = {
    attempt((Lexer.ident <~ "("))
      .verifiedUnexpected(
        "Are you trying to call a function? \n" +
        "Function calls may not appear in expressions and must use `call`"
      )
  }

  lazy val expr: Parsley[Expr] = precedence[Expr](
    // tightest
    _funcCallCheck,
    Ast.IntLit(Lexer.num.label("integer literal")),
    Ast.BoolLit(Lexer.bool.label("boolean literal")),
    Ast.CharLit(Lexer.character.label("character literal")),
    Ast.StrLit(Lexer.str.label("string literal")),
    (Ast.PairLit <# Lexer.pair),
    attempt(Ast.ArrayElem(Ast.Ident(Lexer.ident), some("[" ~> expr <~ "]"))),
    Ast.Ident(Lexer.ident),
    ("(" ~> expr <~ ")").hide
  )(
    // unary precedence 0)
    GOps(Prefix)(
      (Ast.Not <# "!".label("unary operator")),
      (Ast.Neg <# Lexer.negate.label("unary operator")),
      (Ast.Len <# "len"),
      (Ast.Ord <# "ord"),
      (Ast.Chr <# "chr")
    ),

    // binary precendence 1
    GOps(InfixL)((Ast.Mul <# "*"), (Ast.Div <# "/"), (Ast.Mod <# "%")),

    // binary precedence 2
    GOps(InfixL)((Ast.Add <# "+"), (Ast.Sub <# "-")),

    // binary precedence 3
    GOps(InfixL)(
      (Ast.Lte <# "<="),
      (Ast.Lt <# "<"),
      (Ast.Gte <# ">="),
      (Ast.Gt <# ">")
    ),

    // binary precedence 4
    GOps(InfixL)((Ast.Eq <# "=="), (Ast.Neq <# "!=")),

    // binary precedence 5
    GOps(InfixL)((Ast.And <# "&&")),

		// binary precedence 6
		GOps(InfixL) ((Ast.Or <# "||"))
	)//.label("expression")
		
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
