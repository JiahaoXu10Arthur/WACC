package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserStatTest extends AnyFlatSpec{
  "exit statements" should "be parsed" in {
    StatParser.statParse("exit 3 ").get shouldBe Exit(IntLit(3))
    StatParser.statParse("exit true").get shouldBe Exit(BoolLit(true))
    StatParser.statParse("exit 'a'").get shouldBe Exit(CharLit('a'))
    StatParser.statParse("exit \"abc\"").get shouldBe Exit(StrLit("abc"))
    StatParser.statParse("exit null").get shouldBe Exit(PairLit("null"))
    StatParser.statParse("exit _").get shouldBe Exit(Ident("_"))
  }
}
