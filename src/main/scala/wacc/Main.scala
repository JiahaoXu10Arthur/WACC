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
  final private val MAX_PEEPHOLE_LVL   = 3

  final private val waccFileRegex    = """^(?:\w+\/)*\w+\.wacc$""".r
  final private val generalFileRegex = """^(?:\w+\/)*\w+\.\w+$""".r

  sealed trait CmdOpt
  case object PeepholeLvl extends CmdOpt
  case object TailRecOpt  extends CmdOpt
  case object FilePath    extends CmdOpt

  type OptionMap = Map[CmdOpt, Any]

  /* Custom int extractor from string */
  private object Int {
    def unapply(s: String): Option[Int] = util.Try(s.toInt).toOption
  }

  private def getOptions(map: OptionMap, args: List[String]): Try[OptionMap] =
    args match {
      case Nil => Success(map)
      case "-p" :: Int(lvl) :: tail if lvl <= MAX_PEEPHOLE_LVL =>
        getOptions(map + (PeepholeLvl -> lvl), tail)
      case "-p" :: Int(lvl) :: tail if lvl > MAX_PEEPHOLE_LVL =>
        Failure(new IllegalArgumentException("Peephole level must be less than $MAX_PEEPHOLE_LVL"))
      case "-p" :: tail => getOptions(map + (PeepholeLvl -> MAX_PEEPHOLE_LVL), tail)
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

  private def compile(fString: String, fpath: String): Unit = {
    val waccName = fpath.substring(fpath.lastIndexOf("/") + 1).dropRight(WACC_FILE_DROP_LEN)

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
            val path = generateAssembly(ast, st, waccName)
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
          compile(fileString, fpath)
        case None => exitWithCode(ERROR, "ERROR: No file path provided")
      }
    }
  }
}
