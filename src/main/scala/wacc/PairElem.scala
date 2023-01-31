package wacc

import parsley.{Parsley, Success, Failure}
import Parsley.{attempt}
import parsley.combinator.{some, sepBy, sepBy1}
import parsley.implicits.character.{charLift, stringLift}
import ExprParser.{expr}
import Lexer.token
import Ast.{Lvalue, Rvalue, ArrayLiter, ArgList, PairElem}

object PairElem {

	val pair_elem: Parsley[PairElem] = 
		Ast.Pair_Elem((token("fst") <|> token("snd")) ~> lvalue)

	val argList: Parsley[ArgList] =
		Ast.Arg_List(sepBy1(expr, ','))

	val arrayLit: Parsley[ArrayLiter] = 
		Ast.ArrayLit('[' ~> sepBy(expr, ',') <~ ']')

	/* Pair Elem first because it needs to check keywords: fst, snd,
		 then ArryElem need to check []
		 if neither matches, parse as identifier */
	val lvalue: Parsley[Lvalue] = 
		pair_elem <|>
		attempt (Ast.ArrayElem(Lexer.ident <~> some('[' ~> expr <~ ']'))) <|>
		Ast.Ident(Lexer.ident)

	/* Pair Elem first because it needs to check keywords: fst, snd,
		 then check key words for NewPair and Call*/
	val rvalue: Parsley[Rvalue] = 
		pair_elem <|>
		Ast.NewPair("newpair" ~> '(' ~> (expr <~> ',' ~> expr) <~ ')') <|>
		Ast.Call(token("call") ~> Lexer.ident <~> '(' ~> sepBy(expr, ',') <~ ')') <|>
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