package wacc.SyntaxChecker

import parsley.{Parsley, Success, Failure}
import parsley.combinator.sepBy
import parsley.errors.combinator._

import wacc.Ast._
import Lexer.implicitVals._

object StructParser {

  /* 
  struct a of
    type1 field1,
    type2 field2,
    ...
  end
  */

    val field = (TypeParser.type_ <~> Ident(Lexer.ident)).label("struct field")
    
    val struct: Parsley[Struct] = (
        Struct(
          "struct" ~> Ident(Lexer.ident),
          "of" ~> sepBy(field, ",") <~ "end"
        )
    ).label("struct")
    
    def structParse(input: String): Option[Struct] = {
    struct.parse(input) match {
      case Success(x) => {
        Some(x)
      }
      case Failure(msg) => {
        None
      }
    }
  }
}
