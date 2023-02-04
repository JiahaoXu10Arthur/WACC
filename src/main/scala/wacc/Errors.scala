package wacc

import parsley.errors.{ErrorBuilder, tokenextractors}

object Errors {
// Structure checking
  case class SyntaxError(pos: String, lines: SyntaxErrorLines)
  
  sealed trait SyntaxErrorLines
  case class VanillaError(unexpected: String, expecteds: String, reasons: Set[String], lineInfo: String) extends SyntaxErrorLines
  case class SpecialisedError(msgs: Set[String]) extends SyntaxErrorLines
  
  sealed trait SyntaxErrorItem {def get: String}
  case class Unexpected(item: String) extends SyntaxErrorItem {
    def get: String = item
  }
  case class Expected(item: String) extends SyntaxErrorItem {
    def get: String = item
  }
  case object SyntaxEndOfInput extends SyntaxErrorItem {
    def get: String = ""
  }

  class SyntaxErrorBuilder extends ErrorBuilder[SyntaxError] with tokenextractors.MatchParserDemand {

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): String = 
        // line: (begin skp end) errorPointAt: 10 errorWidth: 1  first occurence of error: e
        line ++ s" $errorPointsAt "++s"$errorWidth " .appended(line.charAt(errorPointsAt))

        // Final output format into SyntaxError()
    override def format(pos: Position, source: Source, lines: ErrorInfoLines): SyntaxError = SyntaxError(pos, lines)

    type Position = String
    override def pos(line: Int, col: Int): Position = s"line $line:$col"

    // Source file (ignore)
    type Source = Unit
    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = SyntaxErrorLines
    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines =
        VanillaError(unexpected, expected, reasons, line)

        // Expect: Expected { [, = } Already change to string
    type ExpectedItems = Option[String]
    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = {
        if (alts.isEmpty)
            None
        else 
            Some("Expected { " ++ alts.map(_.get).mkString(", ") ++ " }")
    }

    type Messages = Set[String]
    override def combineMessages(alts: Seq[Message]): Messages = alts.toSet

    // Unexpected: mismatched input 'e'
    type UnexpectedLine = String
    override def unexpected(item: Option[Item]): UnexpectedLine = item match {
        case None => ""
        case Some(value) => "mismatched input \'" ++ value.get ++ "\'"
    }

    type ExpectedLine = String
    override def expected(alts: ExpectedItems): ExpectedLine = alts.get

    override def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = SpecialisedError(msgs)

    type Message = String
    override def reason(reason: String): Message = reason
    override def message(msg: String): Message = msg

    type LineInfo = String

    override val numLinesBefore: Int = 1

    override val numLinesAfter: Int = 1

    type Item = SyntaxErrorItem
    type Raw = Unexpected
    type Named = Expected
    type EndOfInput = SyntaxEndOfInput.type
    override def raw(item: String): Raw = Unexpected(item)
    override def named(item: String): Named = Expected(item)
    override val endOfInput: EndOfInput = SyntaxEndOfInput
  }
}
