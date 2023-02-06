package wacc

object Errors {
  case class WACCError(source: Option[String], pos: (Int, Int), lines: WACCErrorLines) {
    override def toString(): String = {
      val sourceMsg = source match {
        case Some(x) => s"In $x:"
        case None    => ""
      }
      val lineMsg = s"line ${pos._1} : column ${pos._2}\n"
      val errorMsg = sourceMsg ++ lineMsg ++ lines.toString()
      errorMsg
    }
  }

  case class ErrorLineInfo(line: String, errorPointsAt: Int, errorWidth: Int)
  
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
      val lineInfo_ = lineInfoFormat(lineInfo).mkString("\n")
      unexpected_ ++ "\n" ++ expecteds_ ++ "\n" ++ reasons_ ++ lineInfo_
    }
  }

  case class SpecialisedError(msgs: Seq[String], lineInfo: ErrorLineInfo) extends WACCErrorLines {
    override def toString(): String = {
      val msgs_ = msgs.mkString("\n")
      val lineInfo_ = lineInfoFormat(lineInfo).mkString("\n")
      msgs_ ++ lineInfo_
    }
  }

  private def lineInfoFormat(lineInfo: ErrorLineInfo): Seq[String] = {
    Seq(s"$errorLineStart${lineInfo.line}", 
        s"${" " * errorLineStart.length}${errorPointer(lineInfo.errorPointsAt, lineInfo.errorWidth)}")
  }
  private val errorLineStart = ">"
  private def errorPointer(caretAt: Int, caretWidth: Int) = s"${" " * caretAt}${"^" * caretWidth}"
  
  sealed trait WACCErrorItem
  case class WACCRaw(item: String) extends WACCErrorItem {
    override def toString() = item
  }
  case class WACCNamed(item: String) extends WACCErrorItem {
    override def toString() = item
  }
  case object WACCEndOfInput extends WACCErrorItem {
    override def toString: String = ""
  }  
}
