package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._
import BranchLinkTranslator._

class IR (
  val instrs: List[Instruction] = List[Instruction](),
  val strConsts: List[String]  = List[String](),
  val bLInstrs: List[List[Instruction]] = List[List[Instruction]]()
) {
	private val instrsBuffer = mutable.ListBuffer[Instruction]()
	private val strConstsBuffer = mutable.ListBuffer[String]()
	private val bLInstrsBuffer = mutable.ListBuffer[List[Instruction]]()
	
	private val bLNamesBuffer = mutable.ListBuffer[FuncLabel]()

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

	def addBranchLink(bLname: FuncLabel)(implicit ir: IR) = {
		if (!ir.bLNamesBuffer.contains(bLname)){
			ir.bLNamesBuffer += bLname
			ir.bLInstrsBuffer += translateBranchLink(bLname)
		}
	}

	def returnIR()(implicit ir: IR): IR = 
		new IR(ir.instrsBuffer.toList, 
					 ir.strConstsBuffer.toList, 
					 ir.bLInstrsBuffer.toList)

}
