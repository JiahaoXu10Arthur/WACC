package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.expr.{precedence}
import parsley.combinator.{some}
import parsley.errors.combinator._
import parsley.errors.patterns.VerifiedErrors
import parsley.expr.{GOps, Prefix, InfixL, InfixR, InfixN}

import wacc.Ast._
import Lexer.implicitVals._

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
    IntLit(Lexer.num.label("integer literal")),
    BoolLit(Lexer.bool.label("boolean literal")),
    CharLit(Lexer.character.label("character literal")),
    StrLit(Lexer.str.label("string literal")),
    (PairLit <# Lexer.pair),
    attempt(ArrayElem(Ident(Lexer.ident), some("[" ~> expr <~ "]"))),
    attempt(StructElem(Ident(Lexer.ident), some( "." ~> Ident(Lexer.ident)))),  
    Ident(Lexer.ident),
    ("(" ~> expr <~ ")").hide
  )(
    // unary precedence 0)
    GOps(Prefix)(
      (Not <# "!".label("unary operator")),
      (Neg <# Lexer.negate.label("unary operator")),
      (Len <# "len"),
      (Ord <# "ord"),
      (Chr <# "chr")
    ),

    // binary precendence 1
    GOps(InfixL)((Mul <# "*"), (Div <# "/"), (Mod <# "%")),

    // binary precedence 2
    GOps(InfixL)((Add <# "+"),
                (Sub <# "-")),

    // binary precedence 3
    GOps(InfixN)(
      (Lte <# "<="),
      (Lt <# "<"),
      (Gte <# ">="),
      (Gt <# ">")
    ),

    // binary precedence 4
    GOps(InfixN)((Eq <# "=="), (Neq <# "!=")),

    // binary precedence 5
    GOps(InfixR)((And <# "&&")),

    // binary precedence 6
    GOps(InfixR)((Or <# "||"))
  ) .label("expression")

  def exprParse(input: String): Option[Expr] = {
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
