package wacc

import parsley.{ Failure, Success }
import java.nio.file.Files
import java.nio.file.Paths

import wacc.SyntaxChecker.Parser
import wacc.SemanticChecker.SemanticChecker
import wacc.CodeGen.CodeGenerator.generateAssembly
import wacc.Error.Errors.errorsMkString

object Main {
  final private val SUCCESS            = 0
  final private val SYNTAX_ERR         = 100
  final private val SEMANTIC_ERR       = 200
  final private val WACC_FILE_DROP_LEN = 5

  def main(args: Array[String]): Unit = {
    val filename = args.head
    val string   = new String(Files.readAllBytes(Paths.get(filename)))
    val waccName = filename.substring(filename.lastIndexOf("/") + 1).dropRight(WACC_FILE_DROP_LEN)

    println("===== COMPILING =====")

    /* Syntax Check */
    Parser.parse(string) match {
      /* Syntax check success */
      case Success(ast) =>
        /* Semantic Check: keeps the symbol table */
        val (errors, st) = SemanticChecker.semanticCheck(ast)
        errors match {
          /* Semantic check success */
          case errors if errors.isEmpty =>
            println(s"Compile ${args.head} successful!")
            val path = generateAssembly(ast, st, waccName)
            println(s"Assembly file generated at $path")
            System.exit(SUCCESS)

          /* Error detected, semantic error */
          case errors =>
            println(errorsMkString(errors, filename))
            println(s"Exiting with code $SEMANTIC_ERR...")
            System.exit(SEMANTIC_ERR)
        }
      /* Syntax check failed, syntax error */
      case Failure(err) =>
        println(errorsMkString(Seq(err), filename))
        println(s"Exiting with code $SYNTAX_ERR...")
        System.exit(SYNTAX_ERR)
    }

  }
}
