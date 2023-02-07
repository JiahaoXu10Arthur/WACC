package wacc

import scala.io.Source

object Errors {
  case class WACCError(errType: String, source: Option[String], 
                       pos: (Int, Int), lines: WACCErrorLines) {
    override def toString(): String = {
      val errTypeMsg = s"$errType error "
      val sourceMsg = source match {
        case Some(x) => s"in $x:"
        case None    => ""
      }
      val lineMsg = s"at line ${pos._1} : column ${pos._2}\n"
      val errorMsg = errTypeMsg ++ sourceMsg ++ lineMsg ++ lines.toString()
      errorMsg
    }
  }
  
  sealed trait WACCErrorLines
  case class VanillaError(unexpected: Option[WACCErrorItem], 
                          expecteds: Set[WACCErrorItem], 
                          reasons: Seq[String], 
                          lineInfo: ErrorLineInfo) extends WACCErrorLines {
    override def toString(): String = {
      val unexpected_ = unexpected match {
        case None => ""
        case Some(value) => "mismatched input \'" ++ value.toString ++ "\'"
      }
      val expecteds_ = {
        if (expecteds.isEmpty) ""
        else "Expected " ++ expecteds.map(_.toString()).mkString(", ")
      }
      val reasons_ = reasons.mkString("\n")
      val lineInfo_ = lineInfo.toString()
      unexpected_ ++ "\n" ++ expecteds_ ++ "\n" ++ reasons_ ++ "\n" ++ lineInfo_
    }
  }

  case class SpecialisedError(msgs: Seq[String], 
                              lineInfo: ErrorLineInfo) extends WACCErrorLines {
    override def toString(): String = {
      val msgs_ = msgs.mkString("\n")
      val lineInfo_ = lineInfo.toString()
      msgs_ ++ "\n" ++ lineInfo_
    }
  }

  case class ErrorLineInfo(line: String, linesBefore: Seq[String], 
                            linesAfter: Seq[String], errorPointsAt: Int, 
                            errorWidth: Int) {
    override def toString(): String = {
        val seqLines = linesBefore.map(line => s"$errorLineStart$line") ++:
        Seq(s"$errorLineStart$line", s"${" " * errorLineStart.length}${errorPointer(errorPointsAt, errorWidth)}") ++:
        linesAfter.map(line => s"$errorLineStart$line")
        seqLines.mkString("\n")
    }
  }

  private val errorLineStart = ">"
  private def errorPointer(caretAt: Int, caretWidth: Int) = s"${" " * caretAt}${"^" * caretWidth}"
  
  sealed trait WACCErrorItem
  case class WACCRaw(item: String) extends WACCErrorItem {
    override def toString() = s"\"item\""
  }
  case class WACCNamed(item: String) extends WACCErrorItem {
    override def toString() = item
  }
  case object WACCEndOfInput extends WACCErrorItem {
    override def toString: String = ""
  }

  def extractErrorLines(source: String, errorPos: (Int, Int)): Seq[String] = {
    val file = Source.fromFile(source).getLines().toArray
    val length = file.length
    val errorLines =  Seq(file(errorPos._1 - 1), file(errorPos._1), file(errorPos._1 + 1))
    errorLines.foreach(println)
    errorLines
  }
}
