package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._
import Types._
import SymbolObject._

class SemanticExprTest extends AnyFlatSpec {
	val st = new SymbolTable(null)

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
		val st = new SymbolTable(null)
		val testIdent = Ident("ident")(0, 0)

		/* No symbol table, semantic error */
		an [SemanticErr] should be thrownBy testIdent.check(st)

		/* int ident */
		st.add("ident", new VariableObj(IntType()))
		/* After add symbol table, should get type */
		testIdent.check(st) shouldBe IntType()
	}

	"Expr: Array Elem" should "refer type in symbolTable" in {
		val st = new SymbolTable(null)
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

	"Expr: Unary Op: Not" should "return BoolType" in {
		val st = new SymbolTable(null)
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

	"Expr: Unary Op: Neg" should "return IntType" in {
		/* -2 */
		val testNeg1 = Neg(IntLit(1)(0,0))(0,0)
		/* -c */
		val testNeg2 = Neg(CharLit('c')(0,0))(0,0)

		testNeg1.check(st) shouldBe IntType()
		an [SemanticErr] should be thrownBy testNeg2.check(st)
	}

	"Expr: Unary Op: Len" should "return IntType" in {
		val st = new SymbolTable(null)
		/* len(array1) */
		val testLen1 = Len(Ident("array1")(0,0))(0,0)
		/* len(array2) */
		val testLen2 = Len(Ident("array2")(0,0))(0,0)

		an [SemanticErr] should be thrownBy testLen1.check(st)

		/* int array1 */
		st.add("array1", VariableObj(IntType()))
		an [SemanticErr] should be thrownBy testLen1.check(st)

		/* int[] array2 */
		st.add("array2", ArrayObj(IntType(), 3))
		testLen2.check(st) shouldBe IntType()
	}

	"Expr: Unary Op: Ord" should "return IntType" in {
		/* ord(c) */
		val testOrd1 = Ord(CharLit('c')(0,0))(0,0)
		/* ord("string") */
		val testOrd2 = Ord(StrLit("String")(0,0))(0,0)

		testOrd1.check(st) shouldBe IntType()
		an [SemanticErr] should be thrownBy testOrd2.check(st)
	}

	"Expr: Unary Op: Chr" should "return CharType" in {
		/* chr(97) */
		val testChr1 = Chr(IntLit(97)(0,0))(0,0)
		/* chr("string") */
		val testChr2 = Chr(StrLit("String")(0,0))(0,0)

		testChr1.check(st) shouldBe CharType()
		an [SemanticErr] should be thrownBy testChr2.check(st)
	}

	"Expr: Binary Op: Arithmetic" should "return IntType" in {
		/* 1*2 */
		val testbio1 = Mul(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a/true */
		val testbio2 = Div(CharLit('a')(0,0), BoolLit(true)(0,0))(0,0)
		/* "string"+2 */
		val testbio3 = Mul(StrLit("string")(0,0), IntLit(2)(0,0))(0,0)

		testbio1.check(st) shouldBe IntType()
		an [SemanticErr] should be thrownBy testbio2.check(st)
		an [SemanticErr] should be thrownBy testbio3.check(st)
	}

	"Expr: Binary Op: Compare" should "return BoolType" in {
		/* 1 > 2 */
		val testcomp1 = Gt(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a < c */
		val testcomp2 = Lt(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)
		/* "string" >= "this"" */
		val testcomp3 = Gte(StrLit("string")(0,0), StrLit("this")(0,0))(0,0)
		/* 1 <= c */
		val testcomp4 = Gte(IntLit(1)(0,0), CharLit('c')(0,0))(0,0)

		testcomp1.check(st) shouldBe BoolType()
		testcomp2.check(st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy testcomp3.check(st)
		an [SemanticErr] should be thrownBy testcomp4.check(st)
	}

	"Expr: Binary Op: Eq" should "return BoolType" in {
		val st = new SymbolTable(null)
		/* 1 == 2 */
		val testeq1 = Eq(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a != c */
		val testeq2 = Neq(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)
		/* "string" == true */
		val testeq3 = Eq(StrLit("string")(0,0), BoolLit(true)(0,0))(0,0)
		/* "string" != stringIdent */
		val testeq4 = Neq(StrLit("string")(0,0), Ident("stringIdent")(0,0))(0,0)

		testeq1.check(st) shouldBe BoolType()
		testeq2.check(st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy testeq3.check(st)
		an [SemanticErr] should be thrownBy testeq4.check(st)

		/* string stringIdent */
		st.add("stringIdent", VariableObj(StrType()))
		testeq4.check(st) shouldBe BoolType()
	}

	"Expr: Binary Op: Logic" should "return BoolType" in {
		/* true && false */
		val testlog1 = And(BoolLit(true)(0,0), BoolLit(false)(0,0))(0,0)
		/* a || c */
		val testlog2 = Or(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)

		testlog1.check(st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy testlog2.check(st)
	}
}

