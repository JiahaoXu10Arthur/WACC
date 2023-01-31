package wacc

import parsley.{Parsley, Success, Failure}
import parsley.expr.chain
import Types._
import Lexer.implicitVals._
import parsley.lift.{lift2}
import parsley.debug._

object TypeParser {
  
  lazy val type_ = basicType | pairType // | arrayType

  val basicType: Parsley[BasicType] = "int" #> IntType() <|> 
                  "bool" #> BoolType() <|>
                  "char" #> CharType() <|>
                  "string" #> StrType()
  lazy val pairType: Parsley[PairType] = lift2[PairElemType, PairElemType, PairType](
                                    PairType(_, _),
                                    "pair(" ~> pairElemType <~ ",",
                                    pairElemType <~ ")"
                                    )

  lazy val pairElemType = basicType | pairTypeIdent //| arrayType

  val pairTypeIdent: Parsley[PairTypeIdent] = "pair" #> PairTypeIdent()

  //val arrayType: Parsley[ArrayType] = chain.postfix1(type_, "[]" #> (ArrayType))



  def typeParse (input: String): Option[Type] = {
	  (type_).debug("debug").parse(input) match {
      case Success(x) => {
				Some(x)
			}
      case Failure(msg) => {
				None
			}
    }
	}
}
