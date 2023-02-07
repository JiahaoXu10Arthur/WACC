package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserExprTest extends AnyFlatSpec {
    
  "Expr: int liter" should "be parsed as expression" in {
		ExprParser.exprParse("1").get should matchPattern { 
			case IntLit(1) => 
		} 
	}
    
  "Expr: bool liter" should "be parsed as expression" in {
		ExprParser.exprParse("true").get should matchPattern {
			case BoolLit(true) =>
		} 
		ExprParser.exprParse("false").get should matchPattern {
			case BoolLit(false) =>
		} 
	}

  "Expr: char liter" should "be parsed as expression" in {
		ExprParser.exprParse("'a'").get should matchPattern {
			case CharLit('a') =>
		}
	}

  "Expr: string liter" should "be parsed as expression" in {
		ExprParser.exprParse("\"abc\"").get should matchPattern {
			case StrLit("abc") =>
		}
    ExprParser.exprParse("\"t\"").get should matchPattern {
			case StrLit("t") =>
		} 
    ExprParser.exprParse("\"a b c\"").get should matchPattern {
			case StrLit("a b c") =>
		}
	}

  "Expr: pair liter" should "be parsed as expression" in {
    ExprParser.exprParse("null").get should matchPattern {
			case PairLit() =>
		} 
  }

  "Expr: identifier" should "be parsed as expression" in {
    ExprParser.exprParse("_").get should matchPattern {
			case Ident("_") =>
		} 
    ExprParser.exprParse("a").get should matchPattern {
			case Ident("a") =>
		} 
    ExprParser.exprParse("_aBc9").get should matchPattern {
			case Ident("_aBc9") =>
		} 
  }

  "Expr: array element" should "be parsed as expression" in {
    ExprParser.exprParse("array[1]").get should matchPattern {
			case ArrayElem(Ident("array"), List(IntLit(1))) =>
		} 
    ExprParser.exprParse("array[1][5]").get should matchPattern {
			case ArrayElem(Ident("array"), List(IntLit(1), IntLit(5))) =>
		}
  }

  "Expr: unary operator" should "be parsed as expression" in {
    val testString = "\"String\""
    ExprParser.exprParse("!true").get should matchPattern {
			case Not(BoolLit(true)) =>
		} 
    ExprParser.exprParse("!'a'").get should matchPattern {
			case Not(CharLit('a')) =>
		} 
    ExprParser.exprParse("-1").get should matchPattern {
			case IntLit(-1) =>
		} 
    ExprParser.exprParse(s"-$testString").get should matchPattern {
			case Neg(StrLit("String")) =>
		} 
    ExprParser.exprParse(s"len $testString").get should matchPattern {
			case Len(StrLit("String")) =>
		} 
    ExprParser.exprParse("ord 'a'").get should matchPattern {
			case Ord(CharLit('a')) =>
		} 
    ExprParser.exprParse("chr 97").get should matchPattern {
			case Chr(IntLit(97)) =>
		} 
  }

  "Expr: binary operator" should "be parsed as expression" in {
    ExprParser.exprParse("1+2").get should matchPattern {
			case Add(IntLit(1), IntLit(2)) =>
		} 
    ExprParser.exprParse("1-2").get should matchPattern {
			case Sub(IntLit(1), IntLit(2)) =>
		} 
    ExprParser.exprParse("1*2").get should matchPattern {
			case Mul(IntLit(1), IntLit(2)) =>
		} 
    ExprParser.exprParse("1/2").get should matchPattern {
			case Div(IntLit(1), IntLit(2)) =>
		} 
    ExprParser.exprParse("1%2").get should matchPattern {
			case Mod(IntLit(1), IntLit(2)) =>
		} 

    ExprParser.exprParse("5>3").get should matchPattern {
			case Gt(IntLit(5),IntLit(3)) =>
		} 
    ExprParser.exprParse("5>=3").get should matchPattern {
			case Gte(IntLit(5),IntLit(3)) =>
		} 
    ExprParser.exprParse("5<3").get should matchPattern {
			case Lt(IntLit(5),IntLit(3)) =>
		}
    ExprParser.exprParse("5<=3").get should matchPattern {
			case Lte(IntLit(5),IntLit(3)) =>
		} 
    ExprParser.exprParse("5==5").get should matchPattern {
			case Eq(IntLit(5),IntLit(5)) =>
		} 
    ExprParser.exprParse("5!=3").get should matchPattern {
			case Neq(IntLit(5),IntLit(3)) =>
		} 

    ExprParser.exprParse("true&&true").get should matchPattern {
			case And(BoolLit(true),BoolLit(true)) =>
		} 
    ExprParser.exprParse("true||true").get should matchPattern {
			case Or(BoolLit(true),BoolLit(true)) =>
		} 
  }

  "Expr: ( <expr> )" should "be parsed as expression" in {
    ExprParser.exprParse("(1)").get should matchPattern {
			case IntLit(1) =>
		} 
    ExprParser.exprParse("(true)").get should matchPattern {
			case BoolLit(true) =>
		} 
    ExprParser.exprParse("(\"abc\")").get should matchPattern {
			case StrLit("abc") =>
		} 
    ExprParser.exprParse("(null)").get should matchPattern {
			case PairLit() =>
		} 
    ExprParser.exprParse("(_)").get should matchPattern {
			case Ident("_") =>
		} 
  }

}

