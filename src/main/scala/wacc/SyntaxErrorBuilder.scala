package wacc

import Errors._
import parsley.errors.{ErrorBuilder, tokenextractors}

class SyntaxErrorBuilder extends ErrorBuilder[WACCError] with tokenextractors.MatchParserDemand{

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo = {
        Seq(
            s"$errorLineStart${line}", 
            s"${" " * errorLineStart.length}${errorPointer(errorPointsAt, errorWidth)}"
        )
    }

    private val errorLineStart = ">"
    private def errorPointer(caretAt: Int, caretWidth: Int) = s"${" " * caretAt}${"^" * caretWidth}"

    override def format(pos: Position, source: Source, lines: ErrorInfoLines): WACCError = WACCError("Syntax", source, pos, lines)

    type Position = (Int, Int)
    override def pos(line: Int, col: Int): Position = (line, col)

    type Source = Option[String]
    override def source(sourceName: Option[String]): Source = sourceName

    type ErrorInfoLines = WACCErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
        VanillaError(unexpected, expected, reasons, line)

        // Expect: Expected { [, = } Already change to string
    type ExpectedItems = Set[WACCErrorItem]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = alts
   /*     if (alts.isEmpty)
            None
        else 
            Some("Expected { " ++ alts.map(_.get).mkString(", ") ++ " }") */

    type Messages = Seq[String]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSeq

    // Unexpected: mismatched input 'e'
    type UnexpectedLine = Option[WACCErrorItem]
    override def unexpected(item: Option[Item]): UnexpectedLine = item
        /*
        case None => ""
        case Some(value) => "mismatched input \'" ++ value.get ++ "\'"
        */
    type ExpectedLine = Set[WACCErrorItem]
    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs, line)

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = Seq[String]

    override val numLinesBefore: Int = 0

    override val numLinesAfter: Int = 0

    type Item = WACCErrorItem
    type Raw = WACCRaw
    type Named = WACCNamed
    type EndOfInput = WACCEndOfInput.type
    override def raw(item: String): Raw = WACCRaw(item)
    override def named(item: String): Named = WACCNamed(item)
    override val endOfInput: EndOfInput = WACCEndOfInput
}
