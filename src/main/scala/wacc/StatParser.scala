package wacc

import parsley.{Parsley, Success, Failure}
import parsley.combinator.{sepBy1}
import Ast.{Stat}
import Lexer.implicitVals._
import ExprParser.{expr}

object StatParser {
  val exit_ = "exit" ~> Ast.Exit(expr)
  val print_ = "print" ~> Ast.Print(expr)
  val println_ = "println" ~> Ast.Println(expr)
  val free_ = "free" ~> Ast.Free(expr)
  val read_ = "read" ~> Ast.Read(ValueParser.lvalue)
  val ret_ = "return" ~> Ast.Return(expr)
  val skip_ = Ast.Skip <# "skip"

  lazy val begin_ = "begin" ~> Ast.Begin(stmts) <~ "end"
  lazy val if_ = Ast.If("if" ~> expr, "then" ~> stmts, "else" ~> stmts <~ "fi")
  lazy val while_ = Ast.While("while" ~> expr, "do" ~> stmts <~ "done")
  val assign_ = Ast.Assign(ValueParser.lvalue, "=" ~> ValueParser.rvalue)
  val declare_ = Ast.Declare(TypeParser.type_, Ast.Ident(Lexer.ident), 
                             "=" ~> ValueParser.rvalue)


  lazy val stmt: Parsley[Stat] = skip_ | exit_ | print_ | println_ | free_ | 
                                 ret_ | if_ | while_ | begin_ | declare_ | 
                                 assign_ | read_ 
  lazy val stmts: Parsley[List[Stat]] = sepBy1(stmt, ";")

  def statParse(input: String): Option[List[Stat]] = {
    stmts.parse(input) match {
      case Success(x) => Some(x)
      case Failure(msg) => None
    }
  }
}
