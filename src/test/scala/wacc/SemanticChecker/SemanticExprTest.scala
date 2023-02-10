package wacc.SemanticChecker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer

import wacc.Ast._
import wacc.Error.Errors._

import SemanticType._
import SymbolObject._
import SymbolObjectType._
import ExprSemantic._

class SemanticExprTest extends AnyFlatSpec {
	implicit val st = new SymbolTable(null)
	implicit val semErr = new ListBuffer[WACCError]()

	"Expr: int liter" should "be IntType" in {
		val expr = IntLit(1)(0, 0)
		checkExpr(expr) shouldBe IntType()
	}

	"Expr: bool liter" should "be BoolType" in {
		val expr = BoolLit(true)(0, 0)
		checkExpr(expr) shouldBe BoolType()
	}

	"Expr: char liter" should "be CharType" in {
		val expr = CharLit('c')(0, 0)
		checkExpr(expr) shouldBe CharType()
	}

	"Expr: string liter" should "be StrType" in {
		val expr = StrLit("String")(0, 0)
		checkExpr(expr) shouldBe StrType()
	}

	"Expr: Identifer" should "refer type in symbolTable" in {
		val testIdent = Ident("ident")(0, 0)

		/* No symbol table, semantic error */
		checkExpr(testIdent)
		semErr.length shouldBe 1

		/* int ident */
		st.add("ident", VariableType(), new VariableObj(IntType(), (0, 0)))
		/* After add symbol table, should get type */
		checkExpr(testIdent) shouldBe IntType()
	}

	"Expr: Array Elem" should "refer type in symbolTable" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		/* array[1][5] */
		val testArray1 = ArrayElem(Ident("array")(0,0), List(IntLit(1)(0, 0), 
															 IntLit(5)(0, 0))) (0, 0)
		/* array[1][bool] */
		val testArray2 = ArrayElem(Ident("array")(0,0), List(IntLit(1)(0, 0), 
															 BoolLit(true)(0, 0))) (0, 0)

		/* No symbol table, semantic error */
		checkExpr(testArray1)
		semErr.length shouldBe 1
		println(semErr.toString())

		/* TypeObj in symbol table is wrong, semantic error */
		st.add("array", VariableType(), new VariableObj(IntType(), (0, 0)))
		checkExpr(testArray1)
		semErr.length shouldBe 2
		println(semErr.toString())
		
		st.add("array", VariableType(), new VariableObj(ArrayType(ArrayType(IntType())), (0, 0)))
		/* After add symbol table, should get type */
		checkExpr(testArray1) shouldBe IntType()

		/* Not all types in list are int, semantic error */
		checkExpr(testArray2)
		semErr.length shouldBe 3
	}

	"Expr: Unary Op: Not" should "return BoolType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* !true */
		val testNot1 = Not(BoolLit(true)(0,0))(0,0)
		/* !2 */
		val testNot2 = Not(IntLit(2)(0,0))(0,0)

		checkExpr(testNot1) shouldBe BoolType()
		checkExpr(testNot2)
		semErr.length shouldBe 1

		/* bool boolValue = true
			 !boolValue */
		val testNot3 = Not(Ident("boolValue")(0,0))(0,0)
		st.add("boolValue", VariableType(), new VariableObj(BoolType(), (0, 0)))
		checkExpr(testNot3) shouldBe BoolType()
	}

	"Expr: Unary Op: Neg" should "return IntType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* -2 */
		val testNeg1 = Neg(IntLit(1)(0,0))(0,0)
		/* -c */
		val testNeg2 = Neg(CharLit('c')(0,0))(0,0)

		checkExpr(testNeg1) shouldBe IntType()
		checkExpr(testNeg2)
		semErr.length shouldBe 1
	}

	"Expr: Unary Op: Len" should "return IntType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* len(array1) */
		val testLen1 = Len(Ident("array1")(0,0))(0,0)
		/* len(array2) */
		val testLen2 = Len(Ident("array2")(0,0))(0,0)

		checkExpr(testLen1)
		semErr.length shouldBe 1

		/* int array1 */
		st.add("array1", VariableType(), VariableObj(IntType(), (0, 0)))
		checkExpr(testLen1)
		semErr.length shouldBe 2

		/* int[] array2 */
		st.add("array2", VariableType(), VariableObj(ArrayType(IntType()), (0, 0)))
		checkExpr(testLen2) shouldBe IntType()
	}

	"Expr: Unary Op: Ord" should "return IntType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* ord(c) */
		val testOrd1 = Ord(CharLit('c')(0,0))(0,0)
		/* ord("string") */
		val testOrd2 = Ord(StrLit("String")(0,0))(0,0)

		checkExpr(testOrd1) shouldBe IntType()
		checkExpr(testOrd2)
		semErr.length shouldBe 1
	}

	"Expr: Unary Op: Chr" should "return CharType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* chr(97) */
		val testChr1 = Chr(IntLit(97)(0,0))(0,0)
		/* chr("string") */
		val testChr2 = Chr(StrLit("String")(0,0))(0,0)

		checkExpr(testChr1) shouldBe CharType()
		checkExpr(testChr2)
		semErr.length shouldBe 1
	}

	"Expr: Binary Op: Arithmetic" should "return IntType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* 1*2 */
		val testbio1 = Mul(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a/true */
		val testbio2 = Div(CharLit('a')(0,0), BoolLit(true)(0,0))(0,0)
		/* "string"+2 */
		val testbio3 = Mul(StrLit("string")(0,0), IntLit(2)(0,0))(0,0)

		checkExpr(testbio1) shouldBe IntType()
		checkExpr(testbio2)
		semErr.length shouldBe 2
		checkExpr(testbio3)
		semErr.length shouldBe 3
	}

	"Expr: Binary Op: Compare" should "return BoolType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* 1 > 2 */
		val testcomp1 = Gt(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a < c */
		val testcomp2 = Lt(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)
		/* "string" >= "this"" */
		val testcomp3 = Gte(StrLit("string")(0,0), StrLit("this")(0,0))(0,0)
		/* 1 <= c */
		val testcomp4 = Gte(IntLit(1)(0,0), CharLit('c')(0,0))(0,0)

		checkExpr(testcomp1) shouldBe BoolType()
		checkExpr(testcomp2) shouldBe BoolType()
		checkExpr(testcomp3)
		semErr.length shouldBe 2
		checkExpr(testcomp4)
		semErr.length shouldBe 3
	}

	"Expr: Binary Op: Eq" should "return BoolType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		
		/* 1 == 2 */
		val testeq1 = Eq(IntLit(1)(0,0), IntLit(2)(0,0))(0,0)
		/* a != c */
		val testeq2 = Neq(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)
		/* "string" == true */
		val testeq3 = Eq(StrLit("string")(0,0), BoolLit(true)(0,0))(0,0)
		/* "string" != stringIdent */
		val testeq4 = Neq(StrLit("string")(0,0), Ident("stringIdent")(0,0))(0,0)

		checkExpr(testeq1) shouldBe BoolType()
		checkExpr(testeq2) shouldBe BoolType()
		checkExpr(testeq3)
		semErr.length shouldBe 1
		checkExpr(testeq4)
		semErr.length shouldBe 2

		/* string stringIdent */
		st.add("stringIdent", VariableType(), VariableObj(StrType(), (0, 0)))
		checkExpr(testeq4) shouldBe BoolType()
	}

	"Expr: Binary Op: Logic" should "return BoolType" in {
		implicit val st = new SymbolTable(null)
		implicit val semErr = new ListBuffer[WACCError]()
		/* true && false */
		val testlog1 = And(BoolLit(true)(0,0), BoolLit(false)(0,0))(0,0)
		/* a || c */
		val testlog2 = Or(CharLit('a')(0,0), CharLit('c')(0,0))(0,0)

		checkExpr(testlog1) shouldBe BoolType()
		checkExpr(testlog2)
		semErr.length shouldBe 2
	}
}

