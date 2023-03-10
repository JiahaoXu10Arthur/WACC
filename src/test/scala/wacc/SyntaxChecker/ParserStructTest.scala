package wacc.SyntaxChecker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import wacc.Ast._
import Types._

class ParserStructTest extends AnyFlatSpec {

    "Struct: empty" should "be parsed" in {
        StructParser.structParse("struct s of end").get should matchPattern {
            case Struct(Ident("s"), List()) =>
        }
	}

    "Struct: with only int" should "be parsed" in {
        StructParser.structParse("struct s of int a end").get should matchPattern {
            case Struct(Ident("s"), List((Types.IntType(), Ident("a")))) =>
        }
    }

    "Struct: with int and bool" should "be parsed" in {
        StructParser.structParse("struct s of int a, bool b end").get should matchPattern {
            case Struct(Ident("s"), List((Types.IntType(), Ident("a")), (Types.BoolType(), Ident("b")))) =>
        }
    }

    "Struct: with int and bool and char" should "be parsed" in {
        StructParser.structParse("struct s of int a, bool b, char c end").get should matchPattern {
            case Struct(Ident("s"), List((Types.IntType(), Ident("a")),
            (Types.BoolType(), Ident("b")),
            (Types.CharType(), Ident("c")))) =>
        }
    }

    "Struct: with array of int" should "be parsed" in {
        StructParser.structParse("struct s of int[] a end").get should matchPattern {
            case Struct(Ident("s"), List((ArrayType(Types.IntType()), Ident("a")))) =>
        }
    }

    "Struct: with array of bool and char" should "be parsed" in {
        StructParser.structParse("struct s of bool[] a, char[] b end").get should matchPattern {
            case Struct(Ident("s"), 
            List((ArrayType(Types.BoolType()), Ident("a")),
            (ArrayType(Types.CharType()), Ident("b")))) =>
        }
    }
  
}
