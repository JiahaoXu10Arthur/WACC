package wacc.Error

import Errors._
import wacc.SemanticChecker.SemanticTypes

object SemanticErrorBuilder {
  def buildTypeError(
      pos: (Int, Int),
      unexpectedType: SemanticTypes.Type,
      expectedType: Set[SemanticTypes.Type],
      msg: Seq[String]
  ): WACCError = {
    buildWithUnexpected(
      "Type",
      pos,
      unexpectedType.toString(),
      expectedType.map(x => x.toString()),
      msg
    )
  }

  def buildScopeError(
      pos: (Int, Int),
      id: String,
      related: Set[(String, (Int, Int))],
      msg: Seq[String]
  ): WACCError = {
    var relatedString: String = ""
    if (related.nonEmpty) {
      relatedString ++= "Related: \n"
      related.foreach(x =>
        relatedString ++=
          s"  ${x._1} defined at line ${x._2._1}: column ${x._2._2}\n"
      )
    }
    val errorMsg = msg :+ relatedString
    buildWithMsg("Scope", pos, errorMsg)
  }

  def buildFunctionScopeError(
      pos: (Int, Int),
      id: String,
      related: Set[(String, (Int, Int), List[SemanticTypes.Type], SemanticTypes.Type)],
      msg: Seq[String]
  ): WACCError = {
    var relatedString: String = ""
    if (related.nonEmpty) {
      relatedString ++= "Related: \n"
      related.foreach(x =>
        relatedString ++=
          s"  ${x._1} defined at line ${x._2._1}: column ${x._2._2} with " +
          s"argument type ${x._3} and return type ${x._4}\n"
      )
    }
    val errorMsg = msg :+ relatedString
    buildWithMsg("Scope", pos, errorMsg)
  }

  def buildReturnPlacementError(
      pos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildWithMsg("Return placement", pos, msg)
  }

  def buildFuncRedefError(
      pos: (Int, Int),
      errFuncName: String,
      otherFuncPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildRedefError(
      pos,
      "Function redefinition",
      errFuncName,
      otherFuncPos,
      msg
    )
  }

  def buildStructRedefError(
      pos: (Int, Int),
      errStructName: String,
      otherStructPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildRedefError(
      pos,
      "Struct redefinition",
      errStructName,
      otherStructPos,
      msg
    )
  }

  def buildClassRedefError(
      pos: (Int, Int),
      errClassName: String,
      otherClassPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildRedefError(
      pos,
      "Class redefinition",
      errClassName,
      otherClassPos,
      msg
    )
  }

  def buildVarRedefError(
      pos: (Int, Int),
      errVarName: String,
      otherVarPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildRedefError(
      pos,
      "Variable redefinition",
      errVarName,
      otherVarPos,
      msg
    )
  }

  def buildParamRedefError(
      pos: (Int, Int),
      errParamName: String,
      otherParamPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildRedefError(
      pos,
      "Parameter redefinition",
      errParamName,
      otherParamPos,
      msg
    )
  }

  def buildStructFieldRedefError(
      pos: (Int, Int),
      errFieldName: String,
      otherFieldPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    buildRedefError(
      pos,
      "Struct field redefinition",
      errFieldName,
      otherFieldPos,
      msg
    )
  }

  def buildArgNumError(
      pos: (Int, Int),
      unexpectedNum: Int,
      expectedNum: Int,
      msg: Seq[String]
  ): WACCError = {
    val errorMsg = msg :+ s"Unexpected $unexpectedNum arguments\n" +
      s"Expected $expectedNum arguments"
    buildWithMsg("Wrong Arugment Number", pos, errorMsg)
  }

  def buildPairExchangeError(
      pos: (Int, Int)
  ): WACCError = {
    val errorMsg = Seq(
      " Attempting to exchange values between pairs of unknown types",
      " Pair exchange is only legal when the types of at least one of the sides is known or specified"
    )
    buildWithMsg("Invalid Pair Exchange", pos, errorMsg)
  }

  /* Private definitions */
  private def build(
      errorType: String,
      pos: (Int, Int),
      lines: WACCErrorLines
  ): WACCError = {
    WACCError(errorType, pos, lines)
  }

  private def buildWithUnexpected(
      errorType: String,
      pos: (Int, Int),
      unexpected: String,
      expected: Set[String],
      reasons: Seq[String]
  ): WACCError = {
    val unexpected_named = Some(WACCNamed(unexpected))
    val expected_named: Set[WACCErrorItem] = expected.map(WACCNamed(_))
    val lines = VanillaError(
      unexpected_named,
      expected_named,
      reasons,
      LazyLineInfo(pos, 1, 1)
    )
    build(errorType, pos, lines)
  }

  private def buildWithMsg(
      errorType: String,
      pos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    val lines = SpecialisedError(msg, LazyLineInfo(pos, 1, 1))
    build(errorType, pos, lines)
  }

  private def buildRedefError(
      pos: (Int, Int),
      redefType: String,
      errName: String,
      otherPos: (Int, Int),
      msg: Seq[String]
  ): WACCError = {
    val errorMsg = msg :+ "Previously defined at " ++
      s"line ${otherPos._1}: column ${otherPos._2}"
    buildWithMsg(redefType, pos, errorMsg)
  }
}
