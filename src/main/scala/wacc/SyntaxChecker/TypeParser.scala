package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import parsley.expr.chain
import parsley.lift.{lift1, lift2}
import parsley.errors.combinator._
import Parsley.{attempt}

import wacc.Ast._

import Lexer.implicitVals._
import Types._

object TypeParser {

  val basicType: Parsley[BasicType] = attempt(
      "int" #> IntType() |
      "bool" #> BoolType() |
      "char" #> CharType() |
      "string" #> StrType()
  )

  val structType: Parsley[StructType] = attempt(
      lift1[Ident, StructType](
        StructType(_),
        "struct" ~> Ident(Lexer.ident)
      )
  )

  val classType: Parsley[ClassType] = attempt(
      lift1[Ident, ClassType](
        ClassType(_),
        "class" ~> Ident(Lexer.ident)
      )
  )

  val pairTypeIdent: Parsley[PairTypeIdent] = "pair" #> PairTypeIdent()

  val pairType: Parsley[PairType] = attempt(
    lift2[PairElemType, PairElemType, PairType](
      PairType(_, _),
      ("pair(" ~> pairElemType),
      ("," ~> pairElemType <~ ")")
    )
  )

  // label array as `[]` (array type)
  val arrayType: Parsley[ArrayType] = attempt(
    chain.postfix1(classType | structType | basicType | pairType, "[]".label("`[]` (array type)") #> (ArrayType))
  )

  val type_ = attempt(arrayType) | structType | classType | basicType | pairType

  val pairElemType = classType | structType | arrayType | basicType | pairTypeIdent

  def typeParse(input: String): Option[Type] = {
    (type_).parse(input) match {
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
