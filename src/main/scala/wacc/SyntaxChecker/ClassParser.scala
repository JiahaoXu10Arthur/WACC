package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import parsley.combinator.{sepBy, many}
import Parsley.{attempt, lookAhead, notFollowedBy}
import parsley.errors.combinator._

import wacc.Ast._
import Lexer.implicitVals._
import FuncParser.funcs

object ClassParser {
  
  /*
  class a of
      type1 field1,
      type2 field2,
      int f (int a, int b) is
          return ...
      end
  end
  */
  val field = attempt(TypeParser.type_ <~> Ident(Lexer.ident) <~ notFollowedBy("(")).label("class field")
  
  val classStruct: Parsley[Struct] = (
    Struct("class" ~> Ident(Lexer.ident),
    "of" ~> sepBy(field, ",")))

  val waccClass: Parsley[Class] = (
    Class(
      classStruct,
      funcs <~ "end"
    )
  ).label("class")

  private val _classStartCheck = {
    attempt(lookAhead("class" ~> Ident(Lexer.ident) ~> "of"))
  }

  val classes: Parsley[List[Class]] = many(_classStartCheck ~> waccClass)

  def classParse(input: String): Option[Class] = {
    waccClass.parse(input) match {
      case Success(x) => {
        Some(x)
      }
      case Failure(msg) => {
        None
      }
    }
  }
}
