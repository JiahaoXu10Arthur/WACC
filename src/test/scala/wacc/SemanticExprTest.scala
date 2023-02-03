package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._
import Types._
import SymbolObject._
import Ast.st

class SemanticExprTest extends AnyFlatSpec {
	"Expr: int liter" should "be IntType" in {
		IntLit(1)(0, 0).check(st) shouldBe IntType()
	}

	"Expr: bool liter" should "be BoolType" in {
		BoolLit(true)(0, 0).check(st) shouldBe BoolType()
	}

	"Expr: char liter" should "be CharType" in {
		CharLit('c')(0, 0).check(st) shouldBe CharType()
	}

	"Expr: string liter" should "be StrType" in {
		StrLit("String")(0, 0).check(st) shouldBe StrType()
	}

	"Expr: Identifer" should "refer type in symbolTable" in {
		val testIdent = Ident("ident")(0, 0)

		/* No symbol table, semantic error */
		an [SemanticErr] should be thrownBy testIdent.check(st)

		st.add("ident", new VariableObj(IntType()))
		/* After add symbol table, should get type */
		testIdent.check(st) shouldBe IntType()
	}

	"Expr: Array Elem" should "refer type in symbolTable" in {
		/* array[1][5] */
		val testArray1 = ArrayElem(Ident("array")(0,0), List(IntLit(1)(0, 0), 
															 IntLit(5)(0, 0))) (0, 0)
		/* array[1][bool] */
		val testArray2 = ArrayElem(Ident("array")(0,0), List(IntLit(1)(0, 0), 
															 BoolLit(true)(0, 0))) (0, 0)

		/* No symbol table, semantic error */
		an [SemanticErr] should be thrownBy testArray1.check(st)

		/* TypeObj in symbol table is wrong, semantic error */
		st.add("array", new VariableObj(IntType()))
		an [SemanticErr] should be thrownBy testArray1.check(st)
		
		st.add("array", new ArrayObj(IntType(), 3))
		/* After add symbol table, should get type */
		testArray1.check(st) shouldBe IntType()

		/* Not all types in list are int, semantic error */
		an [SemanticErr] should be thrownBy testArray2.check(st)
	}

	"Expr: Unary Operator: Not" should "return BoolType" in {
		/* !true */
		val testNot1 = Not(BoolLit(true)(0,0))(0,0)
		/* !2 */
		val testNot2 = Not(IntLit(2)(0,0))(0,0)

		testNot1.check(st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy testNot2.check(st)

		/* bool boolValue = true
			 !boolValue */
		val testNot3 = Not(Ident("boolValue")(0,0))(0,0)
		st.add("boolValue", new VariableObj(BoolType()))
		testNot3.check(st) shouldBe BoolType()

	}
}
