package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable.ListBuffer

import wacc.SemanticChecker.SymbolTable
import wacc.SyntaxChecker.Types._

import wacc.Error.Errors._
import wacc.Ast._
import wacc.SemanticChecker.FunctionSemantic._

class FunctionOverloadTest extends AnyFlatSpec {

  "Function with different argc" should "be added" in {
		implicit val st: SymbolTable = new SymbolTable(null, null)
		implicit val semErr = new ListBuffer[WACCError]()

		val param1 = new Param(IntType(), Ident("a")(0, 0))(0, 0)
		val param2 = new Param(BoolType(), Ident("b")(0, 0))(0, 0)
		val param3 = new Param(CharType(), Ident("c")(0, 0))(0, 0)

		val func1 = new Func(IntType(), 
												 Ident("func")(0, 0), 
												 List(param1), 
												 List(Skip()(0, 0)))(0, 0)

		val func2 = new Func(IntType(), 
												 Ident("func")(0, 0), 
												 List(param2, param3), 
												 List(Skip()(0, 0)))(0, 0)

		readInFunctionHeader(func1)
		readInFunctionHeader(func2)

		st.lookUpFunc("func").get should have length 2
	}

  "Function with same argc but different type" should "be added" in {
		implicit val st: SymbolTable = new SymbolTable(null, null)
		implicit val semErr = new ListBuffer[WACCError]()

		val param1 = new Param(IntType(), Ident("a")(0, 0))(0, 0)
		val param2 = new Param(BoolType(), Ident("b")(0, 0))(0, 0)
		val param3 = new Param(CharType(), Ident("c")(0, 0))(0, 0)

		val func1 = new Func(IntType(), 
												 Ident("func")(0, 0), 
												 List(param1, param2), 
												 List(Skip()(0, 0)))(0, 0)

		val func2 = new Func(IntType(), 
												 Ident("func")(0, 0), 
												 List(param2, param3), 
												 List(Skip()(0, 0)))(0, 0)

		readInFunctionHeader(func1)
		readInFunctionHeader(func2)

		st.lookUpFunc("func").get should have length 2
	}

  "Function with same type" should "not be added" in {
		implicit val st: SymbolTable = new SymbolTable(null, null)
		implicit val semErr = new ListBuffer[WACCError]()

		val param1 = new Param(IntType(), Ident("a")(0, 0))(0, 0)
		val param2 = new Param(BoolType(), Ident("b")(0, 0))(0, 0)

		val func1 = new Func(IntType(), 
												 Ident("func")(0, 0), 
												 List(param1, param2), 
												 List(Skip()(0, 0)))(0, 0)

		val func2 = new Func(IntType(), 
												 Ident("func")(0, 0), 
												 List(param1, param2), 
												 List(Skip()(0, 0)))(0, 0)

		readInFunctionHeader(func1)
		readInFunctionHeader(func2)

		st.lookUpFunc("func").get should have length 1
		semErr should have size 1
	}
}
