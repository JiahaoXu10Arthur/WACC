package wacc

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Errors {

  // error line position incorrect
  def errorsMkString(erros: Seq[WACCError], source: String): String = {
    val errorString = new StringBuilder()
    errorString.append(s"Errors detected when compiling file ${source}:\n")
    val file = Source.fromFile(source).getLines().toArray

    /* transform the errors to the correct format*/
    for (error <- erros) {
      val errTypeMsg = s"${error.errType} error "
      val lineMsg = s"at line ${error.pos._1} : column ${error.pos._2}\n"
      errorString.append(errTypeMsg ++ lineMsg)
      errorString.append(error.lines.printErrorLines(Some(file)))
      errorString.append("\n\n")
    }
    errorString.toString()
  }

  /* Error definition */
  case class WACCError(
      errType: String,
      pos: (Int, Int),
      lines: WACCErrorLines
  )

  /* Error line definition */
  sealed trait WACCErrorLines {
    def printErrorLines(file: Option[Array[String]]): String
  }
  case class VanillaError(
      unexpected: Option[WACCErrorItem],
      expecteds: Set[WACCErrorItem],
      reasons: Seq[String],
      lineInfo: ErrorLineInfo
  ) extends WACCErrorLines {
    override def printErrorLines(file: Option[Array[String]]): String = {
      val unexpected_ = unexpected match {
        case None        => ""
        case Some(value) => "Unexpected " ++ value.toString
      }
      val expecteds_ = {
        if (expecteds.isEmpty) ""
        else "\nExpected " ++ expecteds.map(_.toString()).mkString(", ")
      }
      val reasons_ = {
        if (reasons.isEmpty) ""
        else "\n" ++ reasons.mkString("\n")
      }
      val lineInfo_ = "\n" ++ lineInfo.printLine(file)
      unexpected_ ++ expecteds_ ++ reasons_ ++ lineInfo_
    }
  }

  case class SpecialisedError(msgs: Seq[String], lineInfo: ErrorLineInfo)
      extends WACCErrorLines {
    override def printErrorLines(file: Option[Array[String]]): String = {
      val msgs_ = msgs.mkString("\n")
      val lineInfo_ = lineInfo.printLine(file)
      msgs_ ++ "\n" ++ lineInfo_
    }
  }

  /* Error line info types */
  sealed trait ErrorLineInfo {
    def printLine(context: Option[Array[String]]): String
  }
  case class StandardLineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
  ) extends ErrorLineInfo {
    override def printLine(context: Option[Array[String]] = None): String = {
      val seqLines = linesBefore.map(line => s"$errorLineStart$line") ++:
        Seq(
          s"$errorLineStart$line",
          s"$errorLineStart${errorPointer(errorPointsAt, errorWidth)}"
        ) ++:
        linesAfter.map(line => s"$errorLineStart$line")
      seqLines.mkString("\n")
    }
  }

  case class LazyLineInfo(
      linePos: (Int, Int),
      numLinesBefore: Int,
      numLinesAfter: Int
  ) extends ErrorLineInfo {
    override def printLine(context: Option[Array[String]]): String =
      extractErrorLines(context.get, linePos, numLinesBefore, numLinesAfter)
        .map(line => s"$errorLineStart$line")
        .mkString("\n")
  }

  private val errorLineStart = "| "
  private def errorPointer(caretAt: Int, caretWidth: Int) =
    s"${" " * caretAt}${"^" * caretWidth}"

  private def extractErrorLines(
      file: Array[String],
      errorPos: (Int, Int),
      numLinesBefore: Int,
      numLinesAfter: Int
  ): Seq[String] = {
    val length = file.length
    val errorLine = file(errorPos._1 - 1)
    val errorLines = ListBuffer(errorLine)
    val pointerStart = errorLine.takeWhile(_.isWhitespace).length
    val pointerWidth = errorLine.trim().length
    errorLines.append(s"${errorPointer(pointerStart, pointerWidth)}")
    for (i <- 1 to numLinesBefore)
      if (errorPos._1 - 1 - i >= 0)
        errorLines.prepend(file(errorPos._1 - 1 - i))
    for (i <- 1 to numLinesAfter)
      if (errorPos._1 - 1 + i < length)
        errorLines.append(file(errorPos._1 - 1 + i))

    errorLines.toSeq
  }

  /* Error item definition */
  sealed trait WACCErrorItem
  case class WACCRaw(item: String) extends WACCErrorItem {
    override def toString() = s"\"${item}\""
  }
  case class WACCNamed(item: String) extends WACCErrorItem {
    override def toString() = item
  }
  case object WACCEndOfInput extends WACCErrorItem {
    override def toString: String = "end of file"
  }
}
