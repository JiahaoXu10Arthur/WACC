package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ParserExprTest extends AnyFlatSpec {
    
  "int liter" should "be parsed" in {
		Parser.exprParse("1") shouldBe true
	}
    
  "bool liter" should "be parsed" in {
		Parser.exprParse("true") shouldBe true
		Parser.exprParse("false") shouldBe true
	}

  "char liter" should "be parsed" in {
		Parser.exprParse('a') shouldBe true
	}

  "string liter" should "be parsed" in {
		Parser.exprParse("abc") shouldBe true
	}

  "pair liter" should "be parsed" in {
		Parser.exprParse("()") shouldBe true
	}

  "identifier" should "be parsed" in pending

  "array element" should "be parsed" in pending

  "unary operator <expr>" should "be parsed" in pending

  "<expr> binary operator <expr>" should "be parsed" in pending

  "( <expr> )" should "be parsed" in pending

}

