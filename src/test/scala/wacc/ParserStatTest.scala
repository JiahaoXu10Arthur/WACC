package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserStatTest extends AnyFlatSpec{
  "begin statements" should "be parsed" in {
    StatParser.statParse("begin end").get shouldBe None
    StatParser.statParse("begin").get shouldBe None
    StatParser.statParse("begin exit 4 end").get shouldBe 
      Begin(List(
          Exit(IntLit(4))
        )
      )
    StatParser.statParse("begin begin skip end end").get shouldBe 
      Begin(List(
          Begin(List(Skip()))
        )
      )
  }
  "exit statements" should "be parsed" in {
    StatParser.statParse("begin exit 3 end").get shouldBe 
      Begin(List(
          Exit(IntLit(3))
        )
      )
    StatParser.statParse("begin exit true end").get shouldBe 
      Begin(List(
          Exit(BoolLit(true))
        )
      )
  }
}
