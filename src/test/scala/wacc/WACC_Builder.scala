package wacc

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

  def buildExitProgram(expr: String): (Program, SymbolTable) = {
    buildProgram(programCode(s"""
      exit $expr
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
}
