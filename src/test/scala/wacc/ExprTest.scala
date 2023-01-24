package wacc

import wacc.ExprParser

class ExprTest extends AbstractTest {

    val compiler = new ExprParser()

    "Empty string parse" should "failed" in {
        compiler.expr_parse("") shouldBe (false, 0)
    }

    "String with letter" should "failed" in {
        compiler.expr_parse("a") shouldBe (false, 0)
    }
    
    "bracket with space" should "failed" in {
        compiler.expr_parse("(1 + 1)") shouldBe (false, 0)
        
    }

    "number with space" should "success and only evaluate the first digit" in {
        compiler.expr_parse("1 + 1") shouldBe (true, 1)
    }

    "Normal plus and minus" should "success and calculate the correct answer" in {
        compiler.expr_parse("1+1") shouldBe (true, 2)
        compiler.expr_parse("2-3") shouldBe (true, -1)
    }
}
