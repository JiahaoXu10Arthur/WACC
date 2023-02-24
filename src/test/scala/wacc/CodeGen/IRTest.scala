package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.Instructions._
import wacc.CodeGen.IR._

class IRTest extends AnyFlatSpec {

  "Empty IR" should "have 3 empty list" in {
		implicit val ir = new IR()
		val immutableIR = returnIR()
		immutableIR.instrs shouldBe empty
		immutableIR.strConsts shouldBe empty
		immutableIR.bLNames shouldBe empty
	}

	"IR lists" should "increase size when add element" in {
		implicit val ir = new IR()

		addInstr(MovInstr(R8, R4))
		addStrConst("String")
		addBLName(DivisionLabel)

		val immutableIR = returnIR()
		immutableIR.instrs should have size 1
		immutableIR.strConsts should have size 1
		immutableIR.bLNames should have size 1
	}
	
	"IR Instrs" should "take duplicate" in {
		implicit val ir = new IR()

		addInstr(MovInstr(R8, R4))
		addInstr(MovInstr(R8, R4))

		val immutableIR = returnIR()
		immutableIR.instrs should have size 2
	}

	"IR strConsts and BLNames" should "remove duplicate" in {
		implicit val ir = new IR()

		addStrConst("String")
		addStrConst("String")
		addBLName(DivisionLabel)
		addBLName(DivisionLabel)

		val immutableIR = returnIR()
		immutableIR.strConsts should have size 1
		immutableIR.bLNames should have size 1
	}

	"IR strConsts" should "retrieve index" in {
		implicit val ir = new IR()

		addStrConst("String")
		addStrConst("Branch")

		val immutableIR = returnIR()
		immutableIR.findStrConstIndex("String") shouldBe 0
		immutableIR.findStrConstIndex("Branch") shouldBe 1
	}

	"IR strConsts" should "retrieve index even have duplicate" in {
		implicit val ir = new IR()

		addStrConst("String")
		addStrConst("String")
		addStrConst("Branch")

		val immutableIR = returnIR()
		immutableIR.findStrConstIndex("String") shouldBe 0
		immutableIR.findStrConstIndex("Branch") shouldBe 1
	}


}
