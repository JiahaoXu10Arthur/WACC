package wacc

import parsley.errors.{ErrorBuilder, tokenextractors}

object Errors {
  case class SyntaxError(pos: (Int, Int), lines: SyntaxErrorLines)
  
  sealed trait SyntaxErrorLines
  case class VanillaError(unexpected: Option[SyntaxErrorItem], expecteds: Set[SyntaxErrorItem], reasons: Set[String]) extends SyntaxErrorLines
  case class SpecialisedError(msgs: Set[String]) extends SyntaxErrorLines
  
  sealed trait SyntaxErrorItem
  case class TestRaw(item: String) extends SyntaxErrorItem
  case class TestNamed(item: String) extends SyntaxErrorItem
  case object TestEndOfInput extends SyntaxErrorItem

  class SyntaxErrorBuilder extends ErrorBuilder[SyntaxError] with tokenextractors.MatchParserDemand {

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): Unit = ???

    override def format(pos: Position, source: Source, lines: ErrorInfoLines): SyntaxError = SyntaxError(pos, lines)

    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Unit
    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = SyntaxErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
        VanillaError(unexpected, expected, reasons) 

    type ExpectedItems = Set[SyntaxErrorItem]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts

    type Messages = Set[String]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    type UnexpectedLine = Option[SyntaxErrorItem]
    override def unexpected(item: Option[Item]): UnexpectedLine = item

    type ExpectedLine = Set[SyntaxErrorItem]
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = ???

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = Unit

    override val numLinesBefore: Int = 0

    override val numLinesAfter: Int = 0

    type Item = SyntaxErrorItem
    type Raw = TestRaw
    type Named = TestNamed
    type EndOfInput = TestEndOfInput.type
    override def raw(item: String): Raw = TestRaw(item)
    override def named(item: String): Named = TestNamed(item)
    override val endOfInput: EndOfInput = TestEndOfInput
  }
}
