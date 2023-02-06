package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Ast._
import SemanticType._
import SymbolObject._
import ExprSemantic._
import SemanticChecker._

class SemanticExprTest extends AnyFlatSpec {
	val st = new SymbolTable(null)

	"Expr: int liter" should "be IntType" in {
		val expr = IntLit(1)(0, 0)
		checkExpr(expr, st) shouldBe IntType()
	}

	"Expr: bool liter" should "be BoolType" in {
		val expr = BoolLit(true)(0, 0)
		checkExpr(expr, st) shouldBe BoolType()
	}

	"Expr: char liter" should "be CharType" in {
		val expr = CharLit('c')(0, 0)
		checkExpr(expr, st) shouldBe CharType()
	}

	"Expr: string liter" should "be StrType" in {
		val expr = StrLit("String")(0, 0)
		checkExpr(expr, st) shouldBe StrType()
	}

	"Expr: Identifer" should "refer type in symbolTable" in {
		val st = new SymbolTable(null)
		val testIdent = Ident("ident")(0, 0)

		/* No symbol table, semantic error */
		an [SemanticErr] should be thrownBy checkExpr(testIdent, st)

		/* int ident */
		st.add("ident", new VariableObj(IntType()))
		/* After add symbol table, should get type */
		checkExpr(testIdent, st) shouldBe IntType()
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
		an [SemanticErr] should be thrownBy checkExpr(testArray1, st)

		/* TypeObj in symbol table is wrong, semantic error */
		st.add("array", new VariableObj(IntType()))
		an [SemanticErr] should be thrownBy checkExpr(testArray1, st)
		
		st.add("array", new ArrayObj(IntType(), 3))
		/* After add symbol table, should get type */
		checkExpr(testArray1, st) shouldBe IntType()

		/* Not all types in list are int, semantic error */
		an [SemanticErr] should be thrownBy checkExpr(testArray2, st)
	}

	"Expr: Unary Op: Not" should "return BoolType" in {
		val st = new SymbolTable(null)
		/* !true */
		val testNot1 = Not(BoolLit(true)(0,0))(0,0)
		/* !2 */
		val testNot2 = Not(IntLit(2)(0,0))(0,0)

		checkExpr(testNot1, st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy checkExpr(testNot2, st)

		/* bool boolValue = true
			 !boolValue */
		val testNot3 = Not(Ident("boolValue")(0,0))(0,0)
		st.add("boolValue", new VariableObj(BoolType()))
		checkExpr(testNot3, st) shouldBe BoolType()
	}

	"Expr: Unary Op: Neg" should "return IntType" in {
		/* -2 */
		val testNeg1 = Neg(IntLit(1)(0,0))(0,0)
		/* -c */
		val testNeg2 = Neg(CharLit('c')(0,0))(0,0)

		checkExpr(testNeg1, st) shouldBe IntType()
		an [SemanticErr] should be thrownBy checkExpr(testNeg2, st)
	}

	"Expr: Unary Op: Len" should "return IntType" in {
		val st = new SymbolTable(null)
		/* len(array1) */
		val testLen1 = Len(Ident("array1")(0,0))(0,0)
		/* len(array2) */
		val testLen2 = Len(Ident("array2")(0,0))(0,0)

		an [SemanticErr] should be thrownBy checkExpr(testLen1, st)

		/* int array1 */
		st.add("array1", VariableObj(IntType()))
		an [SemanticErr] should be thrownBy checkExpr(testLen1, st)

		/* int[] array2 */
		st.add("array2", ArrayObj(IntType(), 3))
		checkExpr(testLen2, st) shouldBe IntType()
	}

	"Expr: Unary Op: Ord" should "return IntType" in {
		/* ord(c) */
		val testOrd1 = Ord(CharLit('c')(0,0))(0,0)
		/* ord("string") */
		val testOrd2 = Ord(StrLit("String")(0,0))(0,0)

		checkExpr(testOrd1, st) shouldBe IntType()
		an [SemanticErr] should be thrownBy checkExpr(testOrd2, st)
	}

	"Expr: Unary Op: Chr" should "return CharType" in {
		/* chr(97) */
		val testChr1 = Chr(IntLit(97)(0,0))(0,0)
		/* chr("string") */
		val testChr2 = Chr(StrLit("String")(0,0))(0,0)

		checkExpr(testChr1, st) shouldBe CharType()
		an [SemanticErr] should be thrownBy checkExpr(testChr2, st)
	}

	"Expr: Binary Op: Arithmetic" should "return IntType" in {
		/* 1*2 */
		val testbio1 = Mul(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a/true */
		val testbio2 = Div(CharLit('a')(0,0), BoolLit(true)(0,0))(0,0)
		/* "string"+2 */
		val testbio3 = Mul(StrLit("string")(0,0), IntLit(2)(0,0))(0,0)

		checkExpr(testbio1, st) shouldBe IntType()
		an [SemanticErr] should be thrownBy checkExpr(testbio2, st)
		an [SemanticErr] should be thrownBy checkExpr(testbio3, st)
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

		checkExpr(testcomp1, st) shouldBe BoolType()
		checkExpr(testcomp2, st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy checkExpr(testcomp3, st)
		an [SemanticErr] should be thrownBy checkExpr(testcomp4, st)
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

		checkExpr(testeq1, st) shouldBe BoolType()
		checkExpr(testeq2, st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy checkExpr(testeq3, st)
		an [SemanticErr] should be thrownBy checkExpr(testeq4, st)

		/* string stringIdent */
		st.add("stringIdent", VariableObj(StrType()))
		checkExpr(testeq4, st) shouldBe BoolType()
	}

	"Expr: Binary Op: Logic" should "return BoolType" in {
		/* true && false */
		val testlog1 = And(BoolLit(true)(0,0), BoolLit(false)(0,0))(0,0)
		/* a || c */
		val testlog2 = Or(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)

		checkExpr(testlog1, st) shouldBe BoolType()
		an [SemanticErr] should be thrownBy checkExpr(testlog2, st)
	}
}

