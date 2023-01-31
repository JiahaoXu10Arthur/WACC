package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._
import Types._

class ParserFuncTest extends AnyFlatSpec{
  "Func: function with double return" should "be parsed" in {
		FuncParser.funcParse(
      "int f() is return 3; return 5 end"
    ).get shouldBe Func(
      IntType(),
      Ident("f"),
      List(),
      List(
        Return(IntLit(3)),
        Return(IntLit(5))
      )
    )
	}

  "Func: function with multiple parameters" should "be parsed" in {
		FuncParser.funcParse(
      """char f(int u, int v, int w, int x, char y, bool z) is
       int i = u + v ;
       int j = w + x ;
       if z
        then return chr (ord y - i * j)
       else return y
       fi
       end"""
    ).get shouldBe Func(
      CharType(),
      Ident("f"),
      List(
        Param(IntType(), Ident("u")),
        Param(IntType(), Ident("v")),
        Param(IntType(), Ident("w")),
        Param(IntType(), Ident("x")),
        Param(CharType(), Ident("y")),
        Param(BoolType(), Ident("z"))
      ),
      List(
        Declare(IntType(), Ident("i"), Add(Ident("u"), Ident("v"))),
        Declare(IntType(), Ident("j"), Add(Ident("w"), Ident("x"))),
        If(
          Ident("z"),
          List(
            Return(
              Chr(
                Sub(
                  Ord(Ident("y")),
                  Mul(Ident("i"), Ident("j"))
                )
              )
            )
          ),
          List(
            Return(Ident("y"))
          )
        )
      )
    )
	}
}
