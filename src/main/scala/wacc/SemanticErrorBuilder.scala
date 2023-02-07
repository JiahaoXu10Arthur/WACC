package wacc

import Errors._

object SemanticErrorBuilder {
  private def build(errorType: String, source: Option[String], pos: (Int, Int), 
            lines: WACCErrorLines): WACCError = {
    WACCError(errorType, source, pos, lines)
  }

  private def buildWithUnexpected(errorType: String, source: Option[String], 
                                  pos: (Int, Int), unexpected: String, 
                                  expected: Set[String], reasons: Seq[String], 
                                  lineInfo: String): WACCError = {
    val unexpected_named = Some(WACCNamed(unexpected))
    val expected_named: Set[WACCErrorItem] = expected.map(WACCNamed(_))
    val lines = VanillaError(unexpected_named, expected_named, 
                             reasons, ErrorLineInfo(lineInfo, Seq(""), Seq(""), 0, 0))
    build(errorType, source, pos, lines)
  }

  private def buildWithMsg(errorType: String, source: Option[String], 
                           pos: (Int, Int), msg: Seq[String], 
                           lineInfo: String): WACCError = {
    val lines = SpecialisedError(msg, ErrorLineInfo(lineInfo, Seq(""), Seq(""), 0, 0))
    build(errorType, source, pos, lines)
  }

  def buildTypeError(source : Option[String], pos: (Int, Int), 
                    unexpectedType: String, expectedType: Set[String], 
                    msg: Seq[String], lineInfo: String): WACCError = {
    buildWithUnexpected("Type", source, pos, unexpectedType, expectedType, 
                        msg, lineInfo)
  }

  def buildScopeError(source: Option[String], pos: (Int, Int), id: String, 
                      related: Set[(String, (Int, Int))], msg: Seq[String], 
                      lineInfo: String): WACCError = {
    val errorMsg = msg :+ "Related: " :++ related.map(
                  x => s"${x._1} defined at line ${x._2._1}: column ${x._2._2}")
    buildWithMsg("Scope", source, pos, errorMsg, lineInfo)
  }

  def buildReturnPlacementError(source: Option[String], pos: (Int, Int), 
                                msg: Seq[String], 
                                lineInfo: String): WACCError = {
    buildWithMsg("Return placement", source, pos, msg, lineInfo)
  }

  def buildFuncRedefError(source: Option[String], pos: (Int, Int), 
                          errFuncName: String, otherFuncPos: (Int, Int), 
                          msg: Seq[String], lineInfo: String): WACCError = {
    buildRedefError(source, pos, "Function redefinition", errFuncName, 
                    otherFuncPos, msg, lineInfo)
  }

  def buildVarRedefError(source: Option[String], pos: (Int, Int), 
                         errVarName: String, otherVarPos: (Int, Int), 
                         msg: Seq[String], lineInfo: String): WACCError = {
    buildRedefError(source, pos, "Variable redefinition", errVarName, 
                    otherVarPos, msg, lineInfo)
  }

  def buildParamRedefError(source: Option[String], pos: (Int, Int), 
                           errParamName: String, otherParamPos: (Int, Int), 
                           msg: Seq[String], lineInfo: String): WACCError = {
    buildRedefError(source, pos, "Parameter redefinition", errParamName, 
                    otherParamPos, msg, lineInfo)
  }
  
  private def buildRedefError(source: Option[String], pos: (Int, Int), 
                              redefType: String, errName: String, 
                              otherPos: (Int, Int), msg: Seq[String], 
                              lineInfo: String): WACCError = {
    val errorMsg = msg :+ "Previously defined at" ++ 
                          s"line ${otherPos._1}: column ${otherPos._2}"
    buildWithMsg(redefType, source, pos, errorMsg, lineInfo)
  }
}

