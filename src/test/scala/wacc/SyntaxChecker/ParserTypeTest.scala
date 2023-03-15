package wacc.SyntaxChecker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.Ast._

import Types._

class ParserTypeTest extends AnyFlatSpec {

  "Base Type: Int" should "be parsed as base type" in {
		TypeParser.typeParse("int").get shouldBe IntType()
	}

	"Base Type: Bool" should "be parsed as base type" in {
		TypeParser.typeParse("bool").get shouldBe BoolType()
	}

	"Base Type: Char" should "be parsed as base type" in {
		TypeParser.typeParse("char").get shouldBe CharType()
	}

	"Base Type: String" should "be parsed as base type" in {
		TypeParser.typeParse("string").get shouldBe StrType()
	}

	"Pair Type: pair" should "be parsed as pair type" in {
		TypeParser.typeParse("pair(int, int)").get shouldBe PairType(IntType(), IntType())
		TypeParser.typeParse("pair(char, string)").get shouldBe PairType(CharType(), StrType())
		TypeParser.typeParse("pair(pair, pair)").get shouldBe PairType(PairTypeIdent(), PairTypeIdent())
	}

	"Array Type: basic array" should "be parsed as array type" in {
		TypeParser.typeParse("int[]").get shouldBe ArrayType(IntType())
		TypeParser.typeParse("char[][]").get shouldBe ArrayType(ArrayType(CharType()))
	}

	"Array Type: pair array" should "be parsed as array type" in {
		TypeParser.typeParse("pair(bool, pair)[]").get shouldBe ArrayType(PairType(BoolType(), PairTypeIdent()))
		TypeParser.typeParse("pair(bool, pair)[][]").get shouldBe ArrayType(ArrayType(PairType(BoolType(), PairTypeIdent())))
		TypeParser.typeParse("pair(bool, int[])[]").get shouldBe ArrayType(PairType(BoolType(), ArrayType(IntType())))
	}

	"Struct Type: struct" should "be parsed as struct type" in {
		TypeParser.typeParse("struct foo").get should matchPattern {
			case StructType(Ident("foo")) =>
		}  
		TypeParser.typeParse("struct foo_bar").get should matchPattern {
			case  StructType(Ident("foo_bar")) =>
		} 
		TypeParser.typeParse("struct foo_bar_123").get should matchPattern {
			case  StructType(Ident("foo_bar_123")) =>
		}
	}

	"Struct Type: pair struct" should "be parsed as struct type" in {
		TypeParser.typeParse("pair(struct a, struct b)").get should matchPattern {
			case PairType(StructType(Ident("a")), StructType(Ident("b"))) =>
		}
		TypeParser.typeParse("pair(struct a, pair)").get should matchPattern {
			case PairType(StructType(Ident("a")), PairTypeIdent()) =>
		}
	}

	"Struct Type: struct array" should "be parsed as struct array type" in {
		TypeParser.typeParse("struct foo[]").get should matchPattern {
			case ArrayType(StructType(Ident("foo"))) =>
		}
		TypeParser.typeParse("struct foo[][]").get should matchPattern {
			case ArrayType(ArrayType(StructType(Ident("foo")))) =>
		}
	}
}
