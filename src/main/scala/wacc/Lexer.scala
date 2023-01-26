package wacc

import parsley.Parsley 
import parsley.token.{Lexer, descriptions}
import descriptions.{LexicalDesc, SpaceDesc}

object Lexer {
  val lexer = new Lexer(LexicalDesc.plain)
}
