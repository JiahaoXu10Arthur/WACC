package wacc

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
                          lineInfo: Seq[String]) extends WACCErrorLines {
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
      val lineInfo_ = lineInfo.mkString("\n")
      unexpected_ ++ "\n" ++ expecteds_ ++ "\n" ++ reasons_ ++ "\n" ++ lineInfo_
    }
  }

  case class SpecialisedError(msgs: Seq[String], lineInfo: Seq[String]) extends WACCErrorLines {
    override def toString(): String = {
      val msgs_ = msgs.mkString("\n")
      val lineInfo_ = lineInfo.mkString("\n")
      msgs_ ++ "\n" ++ lineInfo_
    }
  }
  
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
