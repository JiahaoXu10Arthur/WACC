package wacc

import parsley.token.{Lexer, descriptions}
import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc}
import descriptions.text.{TextDesc, EscapeDesc}
import parsley.implicits.character.{charLift, stringLift}
import parsley.token.predicate

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

  private val desc = LexicalDesc.plain.copy(
    spaceDesc = SpaceDesc.plain.copy(
      commentStart = "#",
      commentEnd = "\n",
      space = predicate.Basic(Character.isWhitespace)
    ),

    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = keywords,
      hardOperators = operators
    ),

    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        literals = escLiterals
      )
    )
    
  )
  val lexer = new Lexer(desc)
  

  /* Definition for literal tokens */
  val num = lexer.lexeme.numeric.unsigned.number32[Int]
  val bool = lexer.lexeme("true" <|> "false")
  val char = lexer.lexeme.text.character.ascii
  val str = lexer.lexeme.text.string.ascii
  val pairLit = lexer.lexeme{"null"}
  val ident = lexer.lexeme.names.identifier
}
