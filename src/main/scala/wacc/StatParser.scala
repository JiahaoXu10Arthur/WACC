package wacc

import parsley.{Parsley, Success, Failure}
import parsley.lift.{lift2}
import parsley.implicits.character.{stringLift}
import parsley.combinator.{sepBy1, skip}
import Ast.{Stat}
import Lexer.{token}
import ExprParser.{expr}
import parsley.debug._
import Parsley.{attempt}

object StatParser {
  val exit = (token("exit") ~> expr).map(Ast.Exit(_))
  val print = (token("print") ~> expr).map(Ast.Print(_))
  //val println = ("println" ~> expr).map(Ast.Println(_))
  val free = (token("free") ~> expr).map(Ast.Free(_))
  val ret = (token("return") ~> expr).map(Ast.Return(_))
  val skip_ = (token("skip") #> Ast.Skip())

  lazy val stmt: Parsley[Stat] = skip_ | exit | print | free | ret
  lazy val stmts: Parsley[List[Stat]] = sepBy1(token(stmt), token(";"))

  def statParse(input: String): Option[Stat] = {
    stmt.debug("sds").parse(input) match {
      case Success(x) => {
        println(x)
        Some(x)
      }
      case Failure(msg) => {
        println(msg)
        None
      }
    }
  }
}
