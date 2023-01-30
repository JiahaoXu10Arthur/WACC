package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserExprTest extends AnyFlatSpec {
    
  "int liter" should "be parsed" in {
		ExprParser.exprParse("1").get shouldBe IntLit(1)
	}
    
  "bool liter" should "be parsed" in {
		ExprParser.exprParse("true").get shouldBe BoolLit(true)
		ExprParser.exprParse("false").get shouldBe BoolLit(false)
	}

  "char liter" should "be parsed" in {
		ExprParser.exprParse("'a'").get shouldBe CharLit('a')
	}

  "string liter" should "be parsed" in {
		ExprParser.exprParse("\"abc\"").get shouldBe StrLit("abc")
    ExprParser.exprParse("\"t\"").get shouldBe StrLit("t")
    ExprParser.exprParse("\"a b c\"").get shouldBe StrLit("a b c")
	}

  "pair liter" should "be parsed" in {
    ExprParser.exprParse("null").get shouldBe PairLit("null")
  }

  "identifier" should "be parsed" in {
    ExprParser.exprParse("_").get shouldBe Ident("_")
    ExprParser.exprParse("a").get shouldBe Ident("a")
    ExprParser.exprParse("_aBc9").get shouldBe Ident("_aBc9")
  }

  "array element" should "be parsed" in pending 

  "unary operator <expr>" should "be parsed" in {
    val testString = "\"String\""
    ExprParser.exprParse("!true").get shouldBe Not(BoolLit(true))
    ExprParser.exprParse("!'a'").get shouldBe Not(CharLit('a'))
    ExprParser.exprParse("-1").get shouldBe Neg(IntLit(1))
    ExprParser.exprParse(s"-$testString").get shouldBe Neg(StrLit("String"))
    ExprParser.exprParse(s"len$testString").get shouldBe Len(StrLit("String"))
    ExprParser.exprParse("ord'a'").get shouldBe Ord(CharLit('a'))
    ExprParser.exprParse("chr97").get shouldBe Chr(IntLit(97))
  }

  "<expr> binary operator <expr>" should "be parsed" in {
    ExprParser.exprParse("1+2").get shouldBe Add(IntLit(1), IntLit(2))
    ExprParser.exprParse("1-2").get shouldBe Sub(IntLit(1), IntLit(2))
    ExprParser.exprParse("1*2").get shouldBe Mul(IntLit(1), IntLit(2))
    ExprParser.exprParse("1/2").get shouldBe Div(IntLit(1), IntLit(2))
    ExprParser.exprParse("1%2").get shouldBe Mod(IntLit(1), IntLit(2))

    ExprParser.exprParse("5>3").get shouldBe Gt(IntLit(5),IntLit(3))
    ExprParser.exprParse("5>=3").get shouldBe Gte(IntLit(5),IntLit(3))
    ExprParser.exprParse("5<3").get shouldBe Lt(IntLit(5),IntLit(3))
    ExprParser.exprParse("5<=3").get shouldBe Lte(IntLit(5),IntLit(3))
    ExprParser.exprParse("5==5").get shouldBe Eq(IntLit(5),IntLit(5))
    ExprParser.exprParse("5!=3").get shouldBe Neq(IntLit(5),IntLit(3))

    ExprParser.exprParse("true&&true").get shouldBe And(BoolLit(true),BoolLit(true))
    ExprParser.exprParse("true||true").get shouldBe Or(BoolLit(true),BoolLit(true))
  }

  "( <expr> )" should "be parsed" in {
    ExprParser.exprParse("(1)").get shouldBe IntLit(1)
    ExprParser.exprParse("(true)").get shouldBe BoolLit(true)
    ExprParser.exprParse("(\"abc\")").get shouldBe StrLit("abc")
    ExprParser.exprParse("(null)").get shouldBe PairLit("null")
    ExprParser.exprParse("(_)").get shouldBe Ident("_")
  }

}

