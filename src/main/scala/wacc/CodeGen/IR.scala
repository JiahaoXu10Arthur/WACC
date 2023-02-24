package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._

class IR (
  val instrs: List[Instruction] = List[Instruction](),
  val strConsts: List[String]  = List[String](),
  val bLNames: List[BranchLinkName] = List[BranchLinkName]()
) {
	private val instrsBuffer = mutable.ListBuffer[Instruction]()
	private val strConstsBuffer = mutable.ListBuffer[String]()
	private val bLNamesBuffer = mutable.ListBuffer[BranchLinkName]()

	def findStrConstIndex(str: String): Int =
		strConsts.indexOf(str)

}

object IR {

	def addInstr(instr: Instruction)(implicit ir: IR) =
		ir.instrsBuffer += instr

	def addStrConst(str: String)(implicit ir: IR) = {
		if (!ir.strConstsBuffer.contains(str)){
			ir.strConstsBuffer += str
		}
	}

	def addBLName(branchLink: BranchLinkName)(implicit ir: IR) = {
		if (!ir.bLNamesBuffer.contains(branchLink)){
			ir.bLNamesBuffer += branchLink
		}
	}

	def returnIR()(implicit ir: IR): IR = 
		new IR(ir.instrsBuffer.toList, 
					 ir.strConstsBuffer.toList, 
					 ir.bLNamesBuffer.toList)

}
