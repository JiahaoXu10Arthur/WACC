package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserStatTest extends AnyFlatSpec{
  "Stat: begin" should "be parsed as statement" in {
    StatParser.statParse("begin end") shouldBe None
    StatParser.statParse("begin") shouldBe None
    StatParser.statParse("begin exit 4 end").get shouldBe 
      List( 
        Begin(
          List( 
            Exit(IntLit(4)) 
          )
        ) 
      )
    StatParser.statParse("begin begin skip end end").get shouldBe 
      List(Begin(List(Begin(List(Skip())))))
  }

  "Stat: exit" should "be parsed as statment" in {
    StatParser.statParse("exit 3").get shouldBe 
      List(
        Exit(IntLit(3))
      )
    StatParser.statParse("exit true").get shouldBe 
      List(
        Exit(BoolLit(true))
      )
  }
}
