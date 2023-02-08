package wacc

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Errors {
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
    }
    errorString.toString()
  }

  /* Error definition */
  case class WACCError(
      errType: String,
      pos: (Int, Int),
      lines: WACCErrorLines
  ) {
    override def toString(): String = {
      val errTypeMsg = s"$errType error "
      val lineMsg = s"at line ${pos._1} : column ${pos._2}\n"
      val errorMsg = errTypeMsg ++ lineMsg ++ lines.toString()
      errorMsg
    }
  }

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
        else "Expected " ++ expecteds.map(_.toString()).mkString(", ")
      }
      val reasons_ = reasons.mkString("\n")
      val lineInfo_ = lineInfo.printLine(file)
      unexpected_ ++ "\n" ++ expecteds_ ++ "\n" ++ reasons_ ++ "\n" ++ lineInfo_
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
          s"${" " * errorLineStart.length}",
          s"${errorPointer(errorPointsAt, errorWidth)}"
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

  private val errorLineStart = ">"
  private def errorPointer(caretAt: Int, caretWidth: Int) =
    s"${" " * caretAt}${"^" * caretWidth}"
  private def extractErrorLines(
      file: Array[String],
      errorPos: (Int, Int),
      numLinesBefore: Int,
      numLinesAfter: Int
  ): Seq[String] = {
    val length = file.length
    val errorLines = ListBuffer(file(errorPos._1))
    for (i <- 1 to numLinesBefore)
      if (errorPos._1 - i >= 0)
        errorLines.prepend(file(errorPos._1 - i))
    for (i <- 1 to numLinesAfter)
      if (errorPos._1 + i < length)
        errorLines.append(file(errorPos._1 + i))

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
    override def toString: String = ""
  }
}
