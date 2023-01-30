package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._

class ParserPairElemTest extends AnyFlatSpec {

  "Identifier" should "be parsed as lvalue" in {
		PairElem.lvalueParse("p").get shouldBe Ident("p")
	}

	"Array Elem" should "be parsed as lvalue" in {
    PairElem.lvalueParse("array[1]").get shouldBe ArrayElem("array", List(IntLit(1)))
    PairElem.lvalueParse("array[1][5]").get shouldBe ArrayElem("array", List(IntLit(1), IntLit(5)))
	}

	"Pair Elem" should "be parsed as lvalue" in {
		PairElem.lvalueParse("fst p").get shouldBe Pair_Elem(Ident("p"))
		PairElem.lvalueParse("snd array[1]").get shouldBe Pair_Elem(ArrayElem("array", List(IntLit(1))))
		PairElem.lvalueParse("fstp").get shouldBe Pair_Elem(Ident("p"))
	}

	"Lvalue Ident" should "be parsed as pair elem" in {
		PairElem.pairElemParse("fst p").get shouldBe Pair_Elem(Ident("p"))
	}

	"Lvalue Array Elem" should "be parsed as pair elem" in {
		PairElem.pairElemParse("snd array[1]").get shouldBe Pair_Elem(ArrayElem("array", List(IntLit(1))))
	}

	"Lvalue Pair Elem" should "be parsed as pair elem" in {
	}

}
