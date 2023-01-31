package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserStatTest extends AnyFlatSpec{

  "Stat: skip" should "be parsed as statement" in {
    StatParser.statParse("skip").get shouldBe List(Skip())
  }

  "Stat: assignment" should "be parsed as statement" in {
    StatParser.statParse("p = 5").get shouldBe List(Assign(Ident("p"), IntLit(5)))
    StatParser.statParse("array[2] = call func ()").get shouldBe List(Assign(
      ArrayElem(Ident("array"), List(IntLit(2))), Call(Ident("func"), List()) ) )
  }

  "Stat: read" should "be parsed as statement" in {
    StatParser.statParse("read p").get shouldBe List(Read(Ident("p")))
    StatParser.statParse("read fst arg").get shouldBe List(Read(Pair_Elem(Ident("arg"))))
  }

  "Stat: free" should "be parsed as statement" in {
    StatParser.statParse("free 1").get shouldBe List(Free(IntLit(1)))
    StatParser.statParse("free p").get shouldBe List(Free(Ident("p")))
  }

  "Stat: return" should "be parsed as statement" in {
    StatParser.statParse("return 0").get shouldBe List(Return(IntLit(0)))
    StatParser.statParse("return true").get shouldBe List(Return(BoolLit(true)))
  }

  "Stat: exit" should "be parsed as statment" in {
    StatParser.statParse("exit 3").get shouldBe List(Exit(IntLit(3)))
    StatParser.statParse("exit true").get shouldBe List(Exit(BoolLit(true)))
  }

  "Stat: print" should "be parsed as statment" in {
    StatParser.statParse("print 3").get shouldBe List(Print(IntLit(3)))
    StatParser.statParse("print 'c'").get shouldBe List(Print(CharLit('c')))
  }

  "Stat: println" should "be parsed as statment" in {
    val string = "\"String\""
    StatParser.statParse("println false").get shouldBe List(Println(BoolLit(false)))
    StatParser.statParse("println" + string).get shouldBe List(Println(StrLit("String")))
  }

  "Stat: if caluse" should "be parsed as statment" in {
    StatParser.statParse("if") shouldBe None
    StatParser.statParse("if then else fi") shouldBe None
    StatParser.statParse("if true then skip fi") shouldBe None

    StatParser.statParse("if true then skip else skip fi").get shouldBe List( If( BoolLit(true), List(Skip()), List(Skip()) ) )
  }

  "Stat: while loop" should "be parsed as statment" in {
    StatParser.statParse("while do done") shouldBe None
    StatParser.statParse("while false do println 5 done").get shouldBe List( While( BoolLit(false), List(Println(IntLit(5))) ) )
  }

  "Stat: begin" should "be parsed as statement" in {
    StatParser.statParse("begin end") shouldBe None
    StatParser.statParse("begin") shouldBe None
    StatParser.statParse("begin exit 4 end").get shouldBe List( Begin(List( Exit(IntLit(4)))) )
    StatParser.statParse("begin begin skip end end").get shouldBe 
      List(Begin(List(Begin(List(Skip())))))
  }

  "Stat: sequence" should "be parsed as statement" in {
    StatParser.statParse(";") shouldBe None
    StatParser.statParse("skip;") shouldBe None
    StatParser.statParse("skip; return 5").get shouldBe List(Skip(), Return(IntLit(5)))
  }


}
