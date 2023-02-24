package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.Instructions._

class StateTableTest extends AnyFlatSpec {
  
	"State table" should "find correct unused variable register" in {
		val st = new StateTable(None)
		st.add("var1", R4)

		st.nextStoreLocation() shouldBe R5

		st.add("var2", st.nextStoreLocation())
		st.add("var3", st.nextStoreLocation())

		st.nextStoreLocation() shouldBe R7
	}

	"State table" should "push to stack if no register available" in {
		val st = new StateTable(None)
		// There should be 4 + 2 variables
		st.updateFPPtr(-8)

		// 4 register used
		st.add("var1", st.nextStoreLocation())
		st.add("var2", st.nextStoreLocation())
		st.add("var3", st.nextStoreLocation())
		st.add("var4", st.nextStoreLocation())

		st.nextStoreLocation() shouldBe RegOffset(FP, -4)

		st.add("var5", st.nextStoreLocation())

		st.nextStoreLocation() shouldBe RegOffset(FP, 0)
	}

	"State table" should "consistent with sub state table" in {
		val st = new StateTable(None)
		st.add("var1", R4)

		// new scope should inherit super scope's state
		val new_st = new StateTable(Some(st))

		new_st.nextStoreLocation() shouldBe R5

		new_st.add("var2", new_st.nextStoreLocation())

		new_st.nextStoreLocation() shouldBe R6
		new_st.add("var3", new_st.nextStoreLocation())

		new_st.nextStoreLocation() shouldBe R7

		// super scope can reuse what used in sub scope
		st.nextStoreLocation() shouldBe R5
	}

}


