package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserProgramTest extends AnyFlatSpec {
  "begin 3 end" should "be parsed" in {
    Parser.parse("begin 3 end").get shouldBe IntLit(3)
  }
}
