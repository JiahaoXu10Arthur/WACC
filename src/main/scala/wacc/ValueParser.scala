package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.combinator.{some, sepBy, sepBy1}
import parsley.lift.{lift1, lift2}
import ExprParser.{expr}
import Ast.{Lvalue, Rvalue, ArrayLiter, ArgList, PairElem}
import Lexer.implicitVals._

object ValueParser {

	val pair_elem: Parsley[PairElem] = 
		Ast.Pair_Elem(("fst" <|> "snd") ~> lvalue)

	val argList: Parsley[ArgList] =
		Ast.Arg_List(sepBy1(expr, ","))

	val arrayLit: Parsley[ArrayLiter] = 
		Ast.ArrayLit("[" ~> sepBy(expr, ",") <~ "]")

	/* Pair Elem first because it needs to check keywords: fst, snd,
		 then ArryElem need to check []
		 if neither matches, parse as identifier */
	val lvalue: Parsley[Lvalue] = 
		pair_elem <|>
		attempt (lift2[Ast.Ident, List[Ast.Expr], Ast.ArrayElem] (
			Ast.ArrayElem(_, _),
			lift1[String, Ast.Ident] (Ast.Ident(_), Lexer.ident),
			(some("[" ~> expr <~ "]"))
		)) <|>
		Ast.Ident(Lexer.ident)

	/* Pair Elem first because it needs to check keywords: fst, snd,
		 then check key words for NewPair and Call*/
	val rvalue: Parsley[Rvalue] = 
		pair_elem <|>
		lift2[Ast.Expr, Ast.Expr, Ast.NewPair] (
			Ast.NewPair(_, _), 
			("newpair" ~> "(" ~> expr), 
			("," ~> expr <~ ")") ) <|>
		lift2[Ast.Ident, List[Ast.Expr], Ast.Call] (
			Ast.Call(_, _), 
			(lift1[String, Ast.Ident] (Ast.Ident(_), "call" ~> Lexer.ident)), 
			("(" ~> sepBy(expr, ",") <~ ")") ) <|>
		arrayLit <|>
		expr

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
		
	def arryLitParse (input: String): Option[ArrayLiter] = {
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