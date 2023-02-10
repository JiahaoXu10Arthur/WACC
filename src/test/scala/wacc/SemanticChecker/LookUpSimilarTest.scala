package wacc.SemanticChecker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import SemanticTypes._
import SymbolObject._
import SymbolObjectType._

class LookUpSimilarTest extends AnyFlatSpec {

	"LookUpSimilar" should "Look up every similar names in scope" in {
	val st = new SymbolTable(null)

	st.add("array", VariableType(), VariableObj(IntType(), (0, 0)))
	st.add("arrAy", VariableType(), VariableObj(IntType(), (2, 3)))

	st.lookUpSimilar("aRRay", VariableType()) should contain theSameElementsAs Seq(("array", (0, 0)), ("arrAy", (2, 3)))
	}

	"LookUpAllSimilar" should "Look up every similar names in all scope" in {
	val st = new SymbolTable(null)

	st.add("array", VariableType(), VariableObj(IntType(), (0, 0)))

	val new_st = new SymbolTable(st)
	new_st.add("arrAy", VariableType(), VariableObj(IntType(), (2, 3)))

	new_st.lookUpAllSimilar("aRRay", VariableType()) should contain theSameElementsAs Seq(("array", (0, 0)), ("arrAy", (2, 3)))
	}

}
