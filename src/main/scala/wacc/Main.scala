package wacc

import java.nio.file.Files
import java.nio.file.Paths
import scala.util.{ Failure, Success, Try }

import wacc.SyntaxChecker.Parser
import wacc.SemanticChecker.SemanticChecker
import wacc.CodeGen.CodeGenerator.generateAssembly
import wacc.Error.Errors.errorsMkString

object Main {
  final private val SUCCESS            = 0
  final private val ERROR              = -1
  final private val SYNTAX_ERR         = 100
  final private val SEMANTIC_ERR       = 200
  final private val WACC_FILE_DROP_LEN = 5

  final private val waccFileRegex    = """^(?:\w+\/)*\w+\.wacc$""".r
  final private val generalFileRegex = """^(?:\w+\/)*\w+\.\w+$""".r

  sealed trait CmdOpt
  case object PeepholeOpt extends CmdOpt
  case object TailRecOpt  extends CmdOpt
  case object FilePath    extends CmdOpt

  type OptionMap = Map[CmdOpt, Any]

  private def getOptions(map: OptionMap, args: List[String]): Try[OptionMap] =
    args match {
      case Nil => Success(map)
      case "-p" :: tail => getOptions(map + (PeepholeOpt -> true), tail)
      case "-t" :: tail => getOptions(map + (TailRecOpt -> true), tail)
      case path :: _ if path.matches(waccFileRegex.regex) =>
        getOptions(map + (FilePath -> path), Nil)
      case path :: _ if path.matches(generalFileRegex.regex) =>
        Failure(new IllegalArgumentException(s"Invalid file type: file $path is not a .wacc file"))
      case opt :: _ => Failure(new IllegalArgumentException(s"Invalid option $opt"))
    }
  
  private def exitWithCode(code: Int, msg: String): Unit = {
    println(msg)
    println(s"Exiting with code $code...")
    System.exit(code)
  }

  private def compile(fString: String, fpath: String, opts: OptionMap): Unit = {
    val waccName = fpath.substring(fpath.lastIndexOf("/") + 1).dropRight(WACC_FILE_DROP_LEN)
    val peephole: Boolean = opts.get(PeepholeOpt) match {
      case Some(b) => b.asInstanceOf[Boolean]
      case None    => false
    }
    val tailrec: Boolean = opts.get(TailRecOpt) match {
      case Some(b) => b.asInstanceOf[Boolean]
      case None    => false
    }

    println("===== COMPILING =====")

    /* Syntax Check */
    Parser.parse(fString) match {
      /* Syntax check success */
      case parsley.Success(ast) =>
        /* Semantic Check: keeps the symbol table */
        val (errors, st) = SemanticChecker.semanticCheck(ast)
        errors match {
          /* Semantic check success */
          case errors if errors.isEmpty => {
            println(s"Compile $fpath successful!")
            /* Generate assembly file */
            val path = generateAssembly(ast, st, waccName, peephole, tailrec)
            exitWithCode(SUCCESS, s"Assembly file generated at $path")
          }

          /* Error detected, semantic error */
          case errors => exitWithCode(SEMANTIC_ERR, errorsMkString(errors, fpath))
        }
      /* Syntax check failed, syntax error */
      case parsley.Failure(err) => exitWithCode(SYNTAX_ERR, errorsMkString(Seq(err), fpath))
    }
  }

  def main(args: Array[String]): Unit = getOptions(Map(), args.toList) match {
    case Failure(e) => exitWithCode(ERROR, s"ERROR: ${e.getMessage}")
    case Success(opts) => {
      opts.get(FilePath) match {
        case Some(path) =>
          val fpath      = path.toString
          val fileString = new String(Files.readAllBytes(Paths.get(fpath)))
          compile(fileString, fpath, opts)
        case None => exitWithCode(ERROR, "ERROR: No file path provided")
      }
    }
  }
}
