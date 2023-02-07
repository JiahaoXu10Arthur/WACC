package wacc

import Errors._

object SemanticErrorBuilder {
  def build(errorType: String, source: Option[String], pos: (Int, Int), 
            lines: WACCErrorLines): WACCError = {
    WACCError(errorType, source, pos, lines)
  }

  def buildWithUnexpected(errorType: String, source: Option[String], pos: (Int, Int), 
                          unexpected: String, expected: Set[String],
                          reasons: Seq[String], lineInfo: String): WACCError = {
    val unexpected_named = Some(WACCNamed(unexpected))
    val expected_named: Set[WACCErrorItem] = expected.map(WACCNamed(_))
    val lines = VanillaError(unexpected_named, expected_named, reasons, Seq(lineInfo))
    build(errorType, source, pos, lines)
  }

  def buildWithMsg(errorType: String, source: Option[String], pos: (Int, Int), 
                   msg: Seq[String], lineInfo: String): WACCError = {
    val lines = SpecialisedError(msg, Seq(lineInfo))
    build(errorType, source, pos, lines)
  }

  def buildTypeError(source : Option[String], pos: (Int, Int), unexpectedType: String, 
                    expectedType: Set[String], msg: Seq[String],
                    lineInfo: String): WACCError = {
    buildWithUnexpected("Type", source, pos, unexpectedType, expectedType, msg, lineInfo)
  }

  def buildScopeError(source: Option[String], pos: (Int, Int), id: String, 
                      related: Set[(String, (Int, Int))], msg: Seq[String], 
                      lineInfo: String): WACCError = {
    val errorMsg = msg :+ "Related: " :++ related.map(
                  x => s"${x._1} defined at line ${x._2._1}: column ${x._2._2}")
    buildWithMsg("Scope", source, pos, errorMsg, lineInfo)
  }

  def buildReturnPlacementError(source: Option[String], pos: (Int, Int), 
                                msg: Seq[String], lineInfo: String): WACCError = {
    buildWithMsg("Return placement", source, pos, msg, lineInfo)
  }

  def buildFuncRedefError(source: Option[String], pos: (Int, Int), 
                          errFuncName: String, otherFuncPos: (Int, Int), 
                          msg: Seq[String], lineInfo: String): WACCError = {
    val errorMsg = msg :+ s"Previously defined at line ${otherFuncPos._1}: column ${otherFuncPos._2}"
    buildWithMsg("Function redefinition", source, pos, errorMsg, lineInfo)
  }

  def buildVarRedefError(source: Option[String], pos: (Int, Int), 
                         errVarName: String, otherVarPos: (Int, Int), 
                         msg: Seq[String], lineInfo: String): WACCError = {
    val errorMsg = msg :+ s"Previously defined at line ${otherVarPos._1}: column ${otherVarPos._2}"
    buildWithMsg("Variable redefinition", source, pos, errorMsg, lineInfo)
  }

  def buildParamRedefError(source: Option[String], pos: (Int, Int), 
                           errParamName: String, otherParamPos: (Int, Int), 
                           msg: Seq[String], lineInfo: String): WACCError = {
    val errorMsg = msg :+ s"Previously defined at line ${otherParamPos._1}: column ${otherParamPos._2}"
    buildWithMsg("Parameter redefinition", source, pos, errorMsg, lineInfo)
  }
}

