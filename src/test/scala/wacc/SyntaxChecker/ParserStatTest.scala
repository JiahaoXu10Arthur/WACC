package wacc.SyntaxChecker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import wacc.Ast._
import Types._

class ParserStatTest extends AnyFlatSpec{

  "Stat: skip" should "be parsed as statement" in {
    StatParser.statParse("skip").get should matchPattern {
			case List(Skip()) =>
		} 
  }

  "Stat: assignment" should "be parsed as statement" in {
    StatParser.statParse("p = 5").get should matchPattern {
			case List(Assign(Ident("p"), IntLit(5))) =>
		}
    StatParser.statParse("array[2] = call func ()").get should matchPattern {
			case List(Assign(
				ArrayElem(Ident("array"), 
				List(IntLit(2))), 
				Call(Ident("func"), List()) ) ) =>
		} 
      
  }

  "Stat: read" should "be parsed as statement" in {
    StatParser.statParse("read p").get should matchPattern {
			case List(Read(Ident("p"))) =>
		}
    StatParser.statParse("read fst arg").get should matchPattern {
			case List(Read(PairElem("fst", Ident("arg")))) =>
		} 
  }

  "Stat: free" should "be parsed as statement" in {
    StatParser.statParse("free 1").get should matchPattern {
			case List(Free(IntLit(1))) =>
		} 
    StatParser.statParse("free p").get should matchPattern {
			case List(Free(Ident("p"))) =>
		}
  }

  "Stat: return" should "be parsed as statement" in {
    StatParser.statParse("return 0").get should matchPattern {
			case List(Return(IntLit(0))) =>
		} 
    StatParser.statParse("return true").get should matchPattern {
			case List(Return(BoolLit(true))) =>
		} 
  }

  "Stat: exit" should "be parsed as statment" in {
    StatParser.statParse("exit 3").get should matchPattern {
			case List(Exit(IntLit(3))) =>
		} 
    StatParser.statParse("exit true").get should matchPattern {
			case List(Exit(BoolLit(true))) =>
		} 
  }

  "Stat: print" should "be parsed as statment" in {
    StatParser.statParse("print 3").get should matchPattern {
			case List(Print(IntLit(3))) =>
		} 
    StatParser.statParse("print 'c'").get should matchPattern {
			case List(Print(CharLit('c'))) =>
		} 
  }

  "Stat: println" should "be parsed as statment" in {
    val string = "\"String\""
    StatParser.statParse("println false").get should matchPattern {
			case List(Println(BoolLit(false))) =>
		} 
    StatParser.statParse("println" + string).get should matchPattern {
			case List(Println(StrLit("String"))) =>
		} 
  }

  "Stat: if caluse" should "be parsed as statment" in {
    StatParser.statParse("if") shouldBe None
    StatParser.statParse("if then else fi") shouldBe None
    StatParser.statParse("if true then skip fi") shouldBe None

    StatParser.statParse("if true then skip else skip fi").get should matchPattern {
			case List( If( BoolLit(true), List(Skip()), List(Skip()) ) ) =>
		} 
  }

  "Stat: while loop" should "be parsed as statment" in {
    StatParser.statParse("while do done") shouldBe None
    StatParser.statParse("while false do println 5 done").get should matchPattern {
			case List( While( BoolLit(false), List(Println(IntLit(5))) ) ) =>
		} 
  }

  "Stat: begin" should "be parsed as statement" in {
    StatParser.statParse("begin end") shouldBe None
    StatParser.statParse("begin") shouldBe None
    StatParser.statParse("begin exit 4 end").get should matchPattern {
			case List( Begin(List( Exit(IntLit(4)))) ) =>
		} 
    StatParser.statParse("begin begin skip end end").get should matchPattern {
			case List(Begin(List(Begin(List(Skip()))))) =>
		}
      
  }

  "Stat: sequence" should "be parsed as statement" in {
    StatParser.statParse(";") shouldBe None
    StatParser.statParse("skip;") shouldBe None
    StatParser.statParse("skip; return 5").get should matchPattern {
			case List(Skip(), Return(IntLit(5))) =>
		} 
  }

  "Stat: declare" should "be parsed as statement" in {
    StatParser.statParse("bool b = true").get should matchPattern {
			case List(Declare(BoolType(), Ident("b"), BoolLit(true))) =>
		}
    StatParser.statParse("int i = 1").get should matchPattern {
			case List(Declare(IntType(), Ident("i"), IntLit(1))) =>
		} 
  }


}
