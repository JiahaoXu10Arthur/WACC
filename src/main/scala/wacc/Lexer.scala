package wacc

import parsley.Parsley
import parsley.character.{char, digit}
import parsley.errors.combinator._
import parsley.token.{Lexer,descriptions}
import parsley.token.errors.{ErrorConfig, FilterConfig, Label, LabelConfig, SpecialisedMessage}
import parsley.token.predicate

import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc, NameDesc, numeric}
import descriptions.text.{TextDesc, EscapeDesc}
import numeric.PlusSignPresence.Optional
import Parsley.{notFollowedBy, attempt}

object Lexer {

  // class of keywords
  private val boolKeywords = Set("true", "false")
  private val pairKeywords = Set("newpair", "pair")
  private val unaryKeywords = Set("len", "ord", "chr")
  private val typeKeywords = Set("int", "char", "bool", "string")
  private val otherKeywords = Set(
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
    "call",
    "int",
    "bool",
    "char",
    "string",
    "null"
  )

  private val keywords =
    boolKeywords ++ pairKeywords ++ unaryKeywords ++ otherKeywords ++ typeKeywords

  // class of operators
  private val binaryOps = Set("-", "*", "/", "%", "+", "&&", "||", ">", ">=", "<", "<=", "==", "!=")
  private val parentheses = Set("(", ")")
  private val squareBrackets = Set("[", "]")

  private val operators = Set(
    ",",
    "=",
    ";",
    "!"
  ) ++ binaryOps ++ parentheses ++ squareBrackets

  private val escLiterals = Set('0', 'b', 't', 'n', 'f', 'r', '\"', '\'', '\\')

  def isAlphaOrUnderscore = predicate.Basic(c => c.isLetter || c == '_')
  def isALphaNumericOrUnderscore =
    predicate.Basic(c => c.isLetterOrDigit || c == '_')

  private val desc = LexicalDesc.plain.copy(
    numericDesc = numeric.NumericDesc.plain.copy(
      positiveSign = Optional
    ),

    // define whitespace and comment
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

    // error message for interger bounds
    override def filterIntegerOutOfBounds(
        min: BigInt,
        max: BigInt,
        nativeRadix: Int
    ): FilterConfig[BigInt] = new SpecialisedMessage[BigInt] {
      def message(n: BigInt) = Seq(
        s"number is not within the range ${min.toString(nativeRadix)} to ${max.toString(nativeRadix)}"
      )
    }

    // provide labels for a set of keywords
    override def labelSymbolKeyword(symbol: String): LabelConfig = {
      if (boolKeywords(symbol))
        Label("boolean literal")
      else if (pairKeywords(symbol))
        Label("pair literal")
      else if (unaryKeywords(symbol))
        Label("unary operator")
      else {
        symbol match {
          case "call"   => Label("function call")
          case "pair"   => Label("pair literal")
          case "int"    => Label("integer type")
          case "bool"   => Label("boolean type")
          case "char"   => Label("character type")
          case "string" => Label("string type")
          case x => Label(s"$x")
        }
      }
    }

    // labels for a set of operators
    override def labelSymbolOperator(symbol: String): LabelConfig =
      if (binaryOps(symbol))
        Label("binary operators")
      else if (parentheses(symbol))
        Label("parentheses")
      else if (squareBrackets(symbol))
        Label("index `[]`")
      else {
        symbol match {
          case "="   => Label("assignment `=`")
          case ","   => Label("comma `,`")
          case ";"   => Label("semicolon `;`")
          case "!"   => Label("unary operator")
        }
      }
  }
  val lexer = new Lexer(desc, errorConfig)

  val identCheck = Seq(lexer.nonlexeme.names.identifier.map(x => s"identifier $x"))

  val keywordsCheck =
    keywords.map(x => lexer.nonlexeme.symbol.softKeyword(x).map(_ => s"keyword $x")).toSeq

  val operatorsCheck =
    (binaryOps ++ Set("!")).map(x => lexer.nonlexeme.symbol.softOperator(x).map(_ => s"operator $x")).toSeq
  
  val concatCheck = Seq(lexer.nonlexeme.symbol.apply("++") #> "++")

  val numCheck = Seq(lexer.nonlexeme.numeric.signed.number32[Int].map(x => s"Integer $x"))

  val parenthesesCheck = parentheses.map(x => lexer.nonlexeme.symbol.softOperator(x).map(_ => "parenthesis")).toSeq

  val squareBracketsCheck = squareBrackets.map(x => lexer.nonlexeme.symbol.softOperator(x).map(_ => "square bracket")).toSeq


  

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
  val negate = attempt(lexer.lexeme(char('-') ~> notFollowedBy(digit))).hide

  val implicitVals = lexer.lexeme.symbol.implicits
}
