package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserPairElemTest extends AnyFlatSpec {

  "Lvalue: Identifier" should "be parsed as lvalue" in {
		ValueParser.lvalueParse("p").get shouldBe Ident("p")
	}

	"Lvalue: Array Elem" should "be parsed as lvalue" in {
    ValueParser.lvalueParse("array[1]").get shouldBe ArrayElem(Ident("array"), List(IntLit(1)))
    ValueParser.lvalueParse("array[1][5]").get shouldBe ArrayElem(Ident("array"), List(IntLit(1), IntLit(5)))
	}

	"Lvalue: Pair Elem" should "be parsed as lvalue" in {
		ValueParser.lvalueParse("fst p").get shouldBe PairElem(Ident("p"))
		ValueParser.lvalueParse("snd array[1]").get shouldBe PairElem(ArrayElem(Ident("array"), List(IntLit(1))))
	}

	"Rvalue: Expression" should "be parsed as rvalue" in {
		ValueParser.rvalueParse("p").get shouldBe Ident("p")
		ValueParser.rvalueParse("1").get shouldBe IntLit(1)
	}

	"Rvalue: ArrayLit" should "be parsed as rvalue" in {
		ValueParser.rvalueParse("[]").get shouldBe ArrayLit(List())
		ValueParser.rvalueParse("[array[5]]").get shouldBe ArrayLit(List(ArrayElem(Ident("array"), List(IntLit(5)))))
		ValueParser.rvalueParse("['p','q','i']").get shouldBe ArrayLit(List(CharLit('p'), CharLit('q'), CharLit('i')))
	}

	"Rvalue: PairElem" should "be parsed as rvalue" in {
		ValueParser.rvalueParse("fst p").get shouldBe PairElem(Ident("p"))
		ValueParser.rvalueParse("snd array[1][5]").get shouldBe PairElem(ArrayElem(Ident("array"), List(IntLit(1), IntLit(5))))
		ValueParser.rvalueParse("fst fst array[1]").get shouldBe(PairElem(PairElem(ArrayElem(Ident("array"), List(IntLit(1))))))
	}

	"Rvalue: NewPair" should "be parsed as rvalue" in {
		ValueParser.rvalueParse("newpair(1,3)").get shouldBe NewPair(IntLit(1), IntLit(3))
		ValueParser.rvalueParse("newpair(player,true)").get shouldBe NewPair(Ident("player"), BoolLit(true))
	}

	"Rvalue: Call" should "be parsed as rvalue" in {
		ValueParser.rvalueParse("call func ()").get shouldBe Call(Ident("func"), List())
		ValueParser.rvalueParse("call func (1,3)").get shouldBe Call(Ident("func"), List(IntLit(1), IntLit(3)))
	}

	"ArgList: Empty" should "be not parsed as array lit" in {
		ValueParser.argListParse("") shouldBe None
	}

	"ArgList: Expressions" should "be not parsed as argLists" in {
		ValueParser.argListParse("array[5]").get shouldBe ArgList(List(ArrayElem(Ident("array"), List(IntLit(5)))))
		ValueParser.argListParse("'p','q','i'").get  shouldBe ArgList(List(CharLit('p'), CharLit('q'), CharLit('i')))
		ValueParser.argListParse("1,true,hello").get  shouldBe ArgList(List(IntLit(1), BoolLit(true), Ident("hello")))
	}

	"ArrayLit: Empty" should "be parsed as array lit" in {
		ValueParser.arryLitParse("[]").get shouldBe ArrayLit(List())
	}

	"ArrayLit: Expressions" should "be parsed as array lit" in {
		ValueParser.arryLitParse("[array[5]]").get shouldBe ArrayLit(List(ArrayElem(Ident("array"), List(IntLit(5)))))
		ValueParser.arryLitParse("['p','q','i']").get shouldBe ArrayLit(List(CharLit('p'), CharLit('q'), CharLit('i')))
		ValueParser.arryLitParse("[1, true, hello]").get shouldBe ArrayLit(List(IntLit(1), BoolLit(true), Ident("hello")))
	}

	"PairElem: Lvalue Ident" should "be parsed as pair elem" in {
		ValueParser.pairElemParse("fst p").get shouldBe PairElem(Ident("p"))
		ValueParser.pairElemParse("snd q").get shouldBe PairElem(Ident("q"))
	}

	"PairElem: Lvalue Array Elem" should "be parsed as pair elem" in {
		ValueParser.pairElemParse("fst array[1]").get shouldBe PairElem(ArrayElem(Ident("array"), List(IntLit(1))))
		ValueParser.pairElemParse("snd array[1][5]").get shouldBe PairElem(ArrayElem(Ident("array"), List(IntLit(1), IntLit(5))))
	}

	"PairElem: Lvalue Pair Elem" should "be parsed as pair elem" in {
		ValueParser.pairElemParse("fst fst array[1]").get shouldBe(PairElem(PairElem(ArrayElem(Ident("array"), List(IntLit(1))))))
		ValueParser.pairElemParse("snd fst q").get shouldBe(PairElem(PairElem(Ident("q"))))
	}

}
