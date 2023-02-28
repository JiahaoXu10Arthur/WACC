package wacc.Utils

import parsley.{Success, Failure}
import wacc.SyntaxChecker.Parser
import wacc.SemanticChecker.SemanticChecker
import wacc.SemanticChecker.SymbolTable
import wacc.Ast.Program

object WACC_Builder {
  private def programCode(body: String) = s"""
    begin
      $body
    end
  """

  private def buildProgram(input: String): (Program, SymbolTable) = {
    Parser.parse(input) match {
      case Success(x) => {
        val (errors, st) = SemanticChecker.semanticCheck(x)
        errors match {
          case errors if errors.isEmpty => (x, st)
          case errors                   => throw new Exception("test program semantically incorrect")
        }
      }
      case Failure(e) => throw new Exception("test program syntactically incorrect")
    }
  }

  def buildProgramWithBody(body: Seq[String]): (Program, SymbolTable) = {
    val bodyStr = body.mkString(";\n")
    println(programCode(bodyStr))
    buildProgram(programCode(bodyStr))
  }

  def buildExitProgram(code: Int): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      exit $code
    """))
  }

  def buildSkipProgram(): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      skip
    """))
  }

  def buildPrintProgram(expr: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      print $expr
    """))
  }

  def buildPrintlnProgram(expr: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      println $expr
    """))
  }

  def buildIntExprProgram(expr: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      int a = $expr;
      exit a
    """))
  }

  def buildBoolExprProgram(expr: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      bool a = $expr;
      print a
    """))
  }

  def buildIfProgram(cond: String, body1: String, body2: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      if $cond 
      then
        $body1
      else
        $body2
      fi
    """))
  }

  def buildWhileProgram(cond: String, body: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      while $cond do
        $body
      done
    """))
  }
}
