package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.combinator.{some, sepBy, sepBy1}
import ExprParser.{expr}
import Ast.{Lvalue, Rvalue, ArrayLit, ArgList, PairElem}
import Lexer.implicitVals._

object ValueParser {

	val pair_elem: Parsley[PairElem] = 
		Ast.PairElem(Lexer.pairElem, lvalue)

	val argList: Parsley[ArgList] =
		Ast.ArgList(sepBy1(expr, ","))

	val arrayLit: Parsley[ArrayLit] = 
		Ast.ArrayLit("[" ~> sepBy(expr, ",") <~ "]")

	/* Pair Elem first because it needs to check keywords: fst, snd,
		 then ArryElem need to check [] if neither matches, parse as identifier */
	val lvalue: Parsley[Lvalue] = 
		pair_elem |
		attempt(Ast.ArrayElem(Ast.Ident(Lexer.ident), some("[" ~> expr <~ "]"))) |
		Ast.Ident(Lexer.ident)

	/* Pair Elem first because it needs to check keywords: fst, snd,
		 then check key words for NewPair and Call*/
	val rvalue: Parsley[Rvalue] = 
		pair_elem |
		Ast.NewPair(("newpair" ~> "(" ~> expr), ("," ~> expr <~ ")") ) |
		Ast.Call(("call" ~> Ast.Ident(Lexer.ident)), "(" ~> sepBy(expr, ",") <~ ")") |
		arrayLit | expr

	def lvalueParse (input: String): Option[Lvalue] = {
		lvalue.parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}

	def rvalueParse (input: String): Option[Rvalue] = {
		rvalue.parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}

	def argListParse (input: String): Option[ArgList] = {
		argList.parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}
		
	def arryLitParse (input: String): Option[ArrayLit] = {
		arrayLit.parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}
	
	def pairElemParse (input: String): Option[PairElem] = {
		pair_elem.parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}
}