package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.Instructions._
import wacc.CodeGen.IRBuilder._

class IRTest extends AnyFlatSpec {

  "Empty IR Builder" should "only have tag as instructions" in {
		implicit val ir = new IRBuilder()
		val immutableIR = returnIR()
		immutableIR.segments should not be empty
		immutableIR.segments.head.instrs should have size 1 // Including text tag
	}

	"IR Builder lists" should "increase size when add element" in {
		implicit val ir = new IRBuilder()

		addInstr(MovInstr(R8, R4))
		addStrConst("String")
		addBranchLink(PrintStr)

		val immutableIR = returnIR()
		immutableIR.segments should have size 2
		val mainSeg = immutableIR.segments.head
		mainSeg.literals should have size 2 // Including data tag
		mainSeg.instrs should have size 2 // Including text tag
	}
	
	"IR Builder instrs" should "take duplicate" in {
		implicit val ir = new IRBuilder()

		addInstr(MovInstr(R8, R4))
		addInstr(MovInstr(R8, R4))

		val immutableIR = returnIR()
		immutableIR.segments should not be empty
		immutableIR.segments.head.instrs should have size 3 // Including text tag
	}

	"IR Builder strConsts and BLNames" should "remove duplicate" in {
		implicit val ir = new IRBuilder()

		addStrConst("String")
		addStrConst("String")
		addBranchLink(PrintStr)
		addBranchLink(PrintStr)
		
		val immutableIR = returnIR()
		immutableIR.segments should have size 2
	}
}
