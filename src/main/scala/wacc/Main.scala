package wacc

import parsley.{Success, Failure}
import java.nio.file.Files
import java.nio.file.Paths

import wacc.SyntaxChecker.Parser
import wacc.SemanticChecker.SemanticChecker
import wacc.Error.Errors.errorsMkString

object Main {
  val SuccessCompile = 0
  val SyntaxError = 100
  val SemanticError = 200

  def main(args: Array[String]): Unit = {
    val filename = args.head
    val string = new String(Files.readAllBytes(Paths.get(filename)))

    /* Syntax Check */
    Parser.parse(string) match {
      /* Syntax check success */
      case Success(x) => {
        /* Semantic Check */
        val (errors, st) = SemanticChecker.semanticCheck(x)
        /* No error reported, compile success */
        if (errors.isEmpty) {
          println(s"${args.head} parse success")
          	System.exit(SuccessCompile)
        } else {
          /* Error detected, semantic error */
          println(s"${args.head} parse fail: ")
          println(errorsMkString(errors, filename))
          	System.exit(SemanticError)
        }
      }
      /* Syntax check failed, syntax error */
      case Failure(err) => {
        println(s"${args.head} parse fail: ")
        println(errorsMkString(Seq(err), filename))
        	System.exit(SyntaxError)
      }
    }

  }
}
