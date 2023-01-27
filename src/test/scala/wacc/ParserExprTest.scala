package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserExprTest extends AnyFlatSpec {
    
  "int liter" should "be parsed" in {
		Parser.exprParse("1").get shouldBe IntLit(1)
	}
    
  "bool liter" should "be parsed" in {
		Parser.exprParse("true").get shouldBe BoolLit(true)
		Parser.exprParse("false").get shouldBe BoolLit(false)
	}

  "char liter" should "be parsed" in {
		Parser.exprParse("'a'").get shouldBe CharLit('a')
	}

  "string liter" should "be parsed" in {
		Parser.exprParse("\"abc\"").get shouldBe StrLit("abc")
	}

  "pair liter" should "be parsed" in {
    Parser.exprParse("null").get shouldBe PairLit("null")
  }

  "identifier" should "be parsed" in {
    Parser.exprParse("_").get shouldBe Ident("_")
    Parser.exprParse("a").get shouldBe Ident("a")
    Parser.exprParse("_aBc9").get shouldBe Ident("_aBc9")
  }

  "array element" should "be parsed" in pending 

  "unary operator <expr>" should "be parsed" in {
    val testString = "\"String\""
    Parser.exprParse("!true").get shouldBe Not(BoolLit(true))
    Parser.exprParse("!'a'").get shouldBe Not(CharLit('a'))
    Parser.exprParse("-1").get shouldBe Neg(IntLit(1))
    Parser.exprParse(s"-$testString").get shouldBe Neg(StrLit("String"))
    Parser.exprParse(s"len$testString").get shouldBe Len(StrLit("String"))
    Parser.exprParse("ord'a'").get shouldBe Ord(CharLit('a'))
    Parser.exprParse("chr97").get shouldBe Chr(IntLit(97))
  }

  "<expr> binary operator <expr>" should "be parsed" in {
    Parser.exprParse("1+2").get shouldBe Add(IntLit(1), IntLit(2))
    Parser.exprParse("1-2").get shouldBe Sub(IntLit(1), IntLit(2))
    Parser.exprParse("1*2").get shouldBe Mul(IntLit(1), IntLit(2))
    Parser.exprParse("1/2").get shouldBe Div(IntLit(1), IntLit(2))
    Parser.exprParse("1%2").get shouldBe Mod(IntLit(1), IntLit(2))

    Parser.exprParse("5>3").get shouldBe Gt(IntLit(5),IntLit(3))
    Parser.exprParse("5>=3").get shouldBe Gte(IntLit(5),IntLit(3))
    Parser.exprParse("5<3").get shouldBe Lt(IntLit(5),IntLit(3))
    Parser.exprParse("5<=3").get shouldBe Lte(IntLit(5),IntLit(3))
    Parser.exprParse("5==5").get shouldBe Eq(IntLit(5),IntLit(5))
    Parser.exprParse("5!=3").get shouldBe Neq(IntLit(5),IntLit(3))

    Parser.exprParse("true&&true").get shouldBe And(BoolLit(true),BoolLit(true))
    Parser.exprParse("true||true").get shouldBe Or(BoolLit(true),BoolLit(true))
  }

  "( <expr> )" should "be parsed" in pending

}

