package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._
import org.scalatest.Suite

class ParserPairElemTest extends AnyFlatSpec {

  "Lvalue: Identifier" should "be parsed as lvalue" in {
		PairElem.lvalueParse("p").get shouldBe Ident("p")
	}

	"Lvalue: Array Elem" should "be parsed as lvalue" in {
    PairElem.lvalueParse("array[1]").get shouldBe ArrayElem("array", List(IntLit(1)))
    PairElem.lvalueParse("array[1][5]").get shouldBe ArrayElem("array", List(IntLit(1), IntLit(5)))
	}

	"Lvalue: Pair Elem" should "be parsed as lvalue" in {
		PairElem.lvalueParse("fst p").get shouldBe Pair_Elem(Ident("p"))
		PairElem.lvalueParse("fstp").get shouldBe Pair_Elem(Ident("p"))
		PairElem.lvalueParse("snd array[1]").get shouldBe Pair_Elem(ArrayElem("array", List(IntLit(1))))
	}

	"ArgList: Empty" should "be not parsed as array lit" in {
		PairElem.argListParse("") shouldBe None
	}

	"ArgList: Expressions" should "be not parsed as argLists" in {
		PairElem.argListParse("array[5]").get shouldBe Arg_List(List(ArrayElem("array", List(IntLit(5)))))
		PairElem.argListParse("'p','q','i'").get  shouldBe Arg_List(List(CharLit('p'), CharLit('q'), CharLit('i')))
		PairElem.argListParse("1,true,hello").get  shouldBe Arg_List(List(IntLit(1), BoolLit(true), Ident("hello")))
	}

	"ArrayLit: Empty" should "be parsed as array lit" in {
		PairElem.arryLitParse("[]").get shouldBe ArrayLit(List())
	}

	"ArrayLit: Expressions" should "be parsed as array lit" in {
		PairElem.arryLitParse("[array[5]]").get shouldBe ArrayLit(List(ArrayElem("array", List(IntLit(5)))))
		PairElem.arryLitParse("['p','q','i']").get shouldBe ArrayLit(List(CharLit('p'), CharLit('q'), CharLit('i')))
		PairElem.arryLitParse("[1,true,hello]").get shouldBe ArrayLit(List(IntLit(1), BoolLit(true), Ident("hello")))
	}

	"PairElem: Lvalue Ident" should "be parsed as pair elem" in {
		PairElem.pairElemParse("fst p").get shouldBe Pair_Elem(Ident("p"))
		PairElem.pairElemParse("snd q").get shouldBe Pair_Elem(Ident("q"))
	}

	"PairElem: Lvalue Array Elem" should "be parsed as pair elem" in {
		PairElem.pairElemParse("fst array[1]").get shouldBe Pair_Elem(ArrayElem("array", List(IntLit(1))))
		PairElem.pairElemParse("snd array[1][5]").get shouldBe Pair_Elem(ArrayElem("array", List(IntLit(1), IntLit(5))))
	}

	"PairElem: Lvalue Pair Elem" should "be parsed as pair elem" in {
		PairElem.pairElemParse("fst fst array[1]").get shouldBe(Pair_Elem(Pair_Elem(ArrayElem("array", List(IntLit(1))))))
		PairElem.pairElemParse("snd fst q").get shouldBe(Pair_Elem(Pair_Elem(Ident("q"))))
	}

}