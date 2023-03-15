package wacc.SyntaxChecker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import wacc.Ast._
import Types._

class PaserClassTest extends AnyFlatSpec {
    "Class: empty" should "be parsed" in {
        ClassParser.classParse("class s of end").get should matchPattern {
            case Class(Struct(Ident("s"), List()), List()) =>
        }
	}

    "Class: with only int" should "be parsed" in {
        ClassParser.classParse("class s of int a end").get should matchPattern {
            case Class(Struct(Ident("s"), List((IntType(), Ident("a")))), List()) =>
        }
    }

    "Class: with a field and a function" should "be parsed" in {
        ClassParser.classParse("class s of int a int f() is return 3 end end").get should matchPattern {
            case Class(Struct(Ident("s"), List((IntType(), Ident("a")))), 
                       List(Func(IntType(), Ident("f"), List(), List(Return(IntLit(3)))))) =>
        }
    }

    "Class: with two fields and two functions" should "be parsed" in {
        ClassParser.classParse("class s of int a, struct b foo int f() is return 3 end struct c g() is return 5 end end").get should matchPattern {
            case Class(Struct(Ident("s"), List((IntType(), Ident("a")), 
                                               (StructType(Ident("b")), Ident("foo")))),
                       List(Func(IntType(), Ident("f"), List(), List(Return(IntLit(3)))), 
                            Func(StructType(Ident("c")), Ident("g"), List(), List(Return(IntLit(5)))))) =>
        }
    }
}
