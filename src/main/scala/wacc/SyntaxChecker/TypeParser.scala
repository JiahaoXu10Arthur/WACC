package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import parsley.expr.chain
import parsley.lift.{lift2}
import parsley.errors.combinator._
import Parsley.{attempt}
import parsley.combinator.{sepBy}

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
      lift2[Ident, List[Type], StructType](
        StructType(_, _),
        "struct" ~> Ident(Lexer.ident),
        "of" ~> sepBy(type_, ",") <~ "end"
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
    chain.postfix1(structType | basicType | pairType, "[]".label("`[]` (array type)") #> (ArrayType))
  )

  val type_ = attempt(arrayType) | structType | basicType | pairType

  val pairElemType = structType | arrayType | basicType | pairTypeIdent

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
