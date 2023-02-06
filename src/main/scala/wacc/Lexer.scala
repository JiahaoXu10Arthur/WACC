package wacc

import parsley.token.{Lexer, descriptions}
import parsley.character.{char, digit}
import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc, NameDesc, numeric}
import descriptions.text.{TextDesc, EscapeDesc}
import numeric.PlusSignPresence.Optional
import parsley.token.predicate
import parsley.Parsley
import Parsley.{notFollowedBy, attempt}



object Lexer {
  private val keywords = Set("true", "false", "begin", "end", "is", "skip", 
                             "read", "free", "return", "exit", "print", 
                             "println", "if", "then", "else", "fi", "while", 
                             "do", "done", "fst", "snd", "newpair", "call", 
                             "int", "bool", "char", "string", "pair", "null")
  private val operators = Set("(", ")", ",", "=", ";", "[", "]", "!", "-", 
                              "len", "ord", "chr", "*", "/", "%", "+", ">", 
                              ">=", "<", "<=", "==", "!=", "&&", "||")
  private val escLiterals = Set('0', 'b', 't', 'n', 'f', 'r', '\"', '\'', '\\')

  def isAlphaOrUnderscore = predicate.Basic(c => c.isLetter || c == '_')
  def isALphaNumericOrUnderscore = predicate.Basic(c => c.isLetterOrDigit || c == '_')

  private val desc = LexicalDesc.plain.copy(
    numericDesc = numeric.NumericDesc.plain.copy(
      positiveSign = Optional,

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
      graphicCharacter = predicate.Basic(c => c >= ' ' && c != '\"' && c != '\\' && c != '\'')
    ),

    nameDesc = NameDesc.plain.copy(
      identifierStart = isAlphaOrUnderscore,
      identifierLetter = isALphaNumericOrUnderscore
    )
    
  )
  val lexer = new Lexer(desc)

  def fully [A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
  

  /* Definition for literal tokens */
  val num = lexer.lexeme.numeric.signed.number32[Int]
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
