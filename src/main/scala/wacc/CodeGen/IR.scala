package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._
import BranchLinkTranslator._

class IR (
  val instrs: List[Instruction] = List(),
  val strConsts: List[CreateLabel]  = List(),
  val bLInstrs: List[List[Instruction]] = List()
) {
	private val instrsBuffer = mutable.ListBuffer[Instruction]()
	private val strConstsBuffer = mutable.ListBuffer[String]()
	private val bLInstrsBuffer = mutable.ListBuffer[List[Instruction]]()
	private val bLNamesBuffer = mutable.ListBuffer[FuncLabel]()
	private var branchCounter = 0
}

object IR {

	def addInstr(instr: Instruction)(implicit ir: IR) =
		ir.instrsBuffer += instr

	def addStrConst(str: String)(implicit ir: IR) = {
		if (!ir.strConstsBuffer.contains(str)){
			ir.strConstsBuffer += str
		}
	}

	def addBranchLink(bLname: FuncLabel)(implicit ir: IR) = {
		if (!ir.bLNamesBuffer.contains(bLname)){
			ir.bLNamesBuffer += bLname
			ir.bLInstrsBuffer += translateBranchLink(bLname)
		}
	}

	def returnIR()(implicit ir: IR): IR = {
		val strConsts = ir.strConstsBuffer.toList.zipWithIndex.map {
			case (str, i) => CreateLabel(StrLabel(s"str$i", str))
		}
		new IR(ir.instrsBuffer.toList, 
					 strConsts.toList, 
					 ir.bLInstrsBuffer.toList)
	}

	def getNextStrId()(implicit ir: IR): Int = {
		ir.strConstsBuffer.size
	}

	def getBranchCounter()(implicit ir: IR): Int = {
		ir.branchCounter
	}

	def incBranchCounter()(implicit ir: IR) = {
		ir.branchCounter += 1
  }

	def findStrConstIndex(str: String)(implicit ir: IR): Int =
		ir.strConstsBuffer.indexOf(str)

	
}
