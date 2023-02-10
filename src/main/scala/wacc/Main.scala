package wacc

import parsley.{Success, Failure}
import java.nio.file.Files
import java.nio.file.Paths

import wacc.SyntaxChecker.Parser
import wacc.SemanticChecker.SemanticChecker
import wacc.Error.Errors.errorsMkString

object Main {
  private final val SUCCESS = 0
  private final val SYNTAX_ERR = 100
  private final val SEMANTIC_ERR = 200

  def main(args: Array[String]): Unit = {
    val filename = args.head
    val string = new String(Files.readAllBytes(Paths.get(filename)))

    println("===== COMPILING =====")

    /* Syntax Check */
    Parser.parse(string) match {
      /* Syntax check success */
      case Success(x) => {
        /* Semantic Check: keeps the symbol table */
        val (errors, st) = SemanticChecker.semanticCheck(x)
        errors match {
          /* Semantic check success */
          case errors if errors.isEmpty => {
            println(s"Compile ${args.head} successful!")
            System.exit(SUCCESS)
          }

          /* Error detected, semantic error */
          case errors => {
            println(errorsMkString(errors, filename))
            println(s"Exiting with code $SEMANTIC_ERR...")
            System.exit(SEMANTIC_ERR)
          }
        }
      }
      /* Syntax check failed, syntax error */
      case Failure(err) => {
        println(errorsMkString(Seq(err), filename))
        println(s"Exiting with code $SYNTAX_ERR...")
        System.exit(SYNTAX_ERR)
      }
    }

  }
}
