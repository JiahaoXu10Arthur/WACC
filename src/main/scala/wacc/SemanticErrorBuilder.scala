package wacc

import Errors._

object SemanticErrorBuilder {
  def build(source: Option[String], pos: (Int, Int), lines: WACCErrorLines): WACCError = {
    WACCError("Semantic", source, pos, lines)
  }

  def buildWithUnexpected(source: Option[String], pos: (Int, Int), 
                          unexpected: String, expected: Set[String],
                          reasons: Seq[String], lineInfo: String): WACCError = {
    val unexpected_named = Some(WACCNamed(unexpected))
    val expected_named: Set[WACCErrorItem] = expected.map(WACCNamed(_))
    val lines = VanillaError(unexpected_named, expected_named, reasons, Seq(lineInfo))
    build(source, pos, lines)
  }

  def buildWithMsg(source: Option[String], pos: (Int, Int), msg: Seq[String],
                   lineInfo: String): WACCError = {
    val lines = SpecialisedError(msg, Seq(lineInfo))
    build(source, pos, lines)
  }
}

