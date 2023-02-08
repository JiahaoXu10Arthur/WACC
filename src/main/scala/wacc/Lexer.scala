package wacc

import parsley.Parsley
import parsley.character.char
import parsley.character.digit
import parsley.errors.combinator._
import parsley.token.Lexer
import parsley.token.descriptions
import parsley.token.errors.ErrorConfig
import parsley.token.errors.FilterConfig
import parsley.token.errors.Label
import parsley.token.errors.LabelConfig
import parsley.token.errors.SpecialisedMessage
import parsley.token.predicate

import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc, NameDesc, numeric}
import descriptions.text.{TextDesc, EscapeDesc}
import numeric.PlusSignPresence.Optional
import Parsley.{notFollowedBy, attempt}

object Lexer {
  private val keywords = Set(
    "true",
    "false",
    "begin",
    "end",
    "is",
    "skip",
    "read",
    "free",
    "return",
    "exit",
    "print",
    "println",
    "if",
    "then",
    "else",
    "fi",
    "while",
    "do",
    "done",
    "fst",
    "snd",
    "newpair",
    "call",
    "int",
    "bool",
    "char",
    "string",
    "pair",
    "null",
    "len",
    "ord",
    "chr"
  )
  private val arithmeticOps = Set("-", "*", "/", "%", "+")
  private val boolOps = Set("!", "&&", "||")
  private val compareOps = Set(">", ">=", "<", "<=", "==", "!=")
  private val parensOps = Set("(", ")")
  private val indexOps = Set("[", "]")
                             
  private val operators = Set(",", "=", ";") ++ arithmeticOps ++ boolOps ++ compareOps ++ parensOps ++ indexOps
  private val escLiterals = Set('0', 'b', 't', 'n', 'f', 'r', '\"', '\'', '\\')

  def isAlphaOrUnderscore = predicate.Basic(c => c.isLetter || c == '_')
  def isALphaNumericOrUnderscore =
    predicate.Basic(c => c.isLetterOrDigit || c == '_')

  private val desc = LexicalDesc.plain.copy(
    numericDesc = numeric.NumericDesc.plain.copy(
      positiveSign = Optional
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      commentLineAllowsEOF = true,
      space = predicate.Basic(Character.isWhitespace)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = keywords,
      hardOperators = operators
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = escLiterals,
        gapsSupported = false
      ),
      characterLiteralEnd = '\'',
      stringEnds = Set("\""),
      graphicCharacter =
        predicate.Basic(c => c >= ' ' && c != '\"' && c != '\\' && c != '\'')
    ),
    nameDesc = NameDesc.plain.copy(
      identifierStart = isAlphaOrUnderscore,
      identifierLetter = isALphaNumericOrUnderscore
    )
  )
  private val errorConfig = new ErrorConfig {
    override def filterIntegerOutOfBounds(
        min: BigInt,
        max: BigInt,
        nativeRadix: Int
    ): FilterConfig[BigInt] = new SpecialisedMessage[BigInt] {
      def message(n: BigInt) = Seq(
        s"number is not within the range ${min.toString(nativeRadix)} to ${max.toString(nativeRadix)}"
      )
    }

    // TODO: ask if can explain

    override def labelSymbolOperator(symbol: String): LabelConfig = 
      if (arithmeticOps(symbol))
        Label("arithmetic operators")
      else if (boolOps(symbol))
        Label("boolean operators")
      else if (parensOps(symbol))
        Label("parentheses")
      else if (compareOps(symbol))
        Label("compare operators")
      else if (indexOps(symbol))
        Label("index `[]`")
      else {
        symbol match {
          case "=" => Label("assignment")
          case "," => Label("comma")
          case ";" => Label("semicolon")
          case "len" => Label("length operator")
          case "ord" => Label("ordinal operator")
          case "chr" => Label("character operator")
        }
      }
  }
  val lexer = new Lexer(desc, errorConfig)

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  /* Definition for literal tokens */
  val num = lexer.lexeme.numeric.signed.number32[Int].label("number")
  val bool = lexer.lexeme.symbol("true") #> true |
    lexer.lexeme.symbol("false") #> false
  val character = lexer.lexeme.text.character.ascii
  val str = lexer.lexeme.text.string.ascii
  val pair = lexer.lexeme.symbol("null")
  val ident = lexer.lexeme.names.identifier
  val pairElem = lexer.lexeme.symbol("fst") #> "fst" |
    lexer.lexeme.symbol("snd") #> "snd"
  val negate = attempt(lexer.lexeme(char('-') ~> notFollowedBy(digit)))

  val implicitVals = lexer.lexeme.symbol.implicits
}
