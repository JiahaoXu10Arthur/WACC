package wacc

import parsley.{Parsley, Success, Failure}
import Types._
import parsley.expr.chain
import parsley.lift.{lift2}
import parsley.debug._
import Parsley.{attempt}
import Lexer.implicitVals._

object TypeParser {

  val basicType: Parsley[BasicType] = "int" #> IntType() <|> 
                  "bool" #> BoolType() <|>
                  "char" #> CharType() <|>
                  "string" #> StrType()

  val pairTypeIdent: Parsley[PairTypeIdent] = "pair" #> PairTypeIdent()

  val pairType: Parsley[PairType] = lift2[PairElemType, PairElemType, PairType](
                                  PairType(_, _),
                                  ("pair(" ~> pairElemType),
                                  ("," ~> pairElemType <~ ")")
                                  )

  val arrayType: Parsley[ArrayType] 
    = attempt(chain.postfix1(basicType | pairType, "[]" #> (ArrayType)))

  val type_ = arrayType | basicType | pairType 

  val pairElemType = arrayType | basicType | pairTypeIdent

  

  def typeParse (input: String): Option[Type] = {
	  (type_).debug("debug").parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
        println(msg)
				None
			}
    }
	}
}