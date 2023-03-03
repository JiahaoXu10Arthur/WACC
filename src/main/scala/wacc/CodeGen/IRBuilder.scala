package wacc.CodeGen

import scala.collection.mutable.{ListBuffer, Set}
import wacc.Instructions._
import BranchLinkTranslator._

class IRBuilder()
{
  private val instrsBuffer    = ListBuffer[Instruction]()
  private val strConstsBuffer = ListBuffer[String]()
  private val bLInstrsBuffer  = ListBuffer[IRSegment]()
  private val bLNamesBuffer   = Set[FuncLabel]()
  private var branchCounter   = 0
}

object IRBuilder {
  def addInstr(instr: Instruction)(implicit ir: IRBuilder) =
    ir.instrsBuffer += instr

  def addStrConst(str: String)(implicit ir: IRBuilder) =
    if (!ir.strConstsBuffer.contains(str)) {
      ir.strConstsBuffer += str
    }

  def addBranchLink(bLName: FuncLabel)(implicit ir: IRBuilder): Unit =
    if (!ir.bLNamesBuffer.contains(bLName)) {
      ir.bLNamesBuffer += bLName
      translateBranchLink(bLName) match {
        case Some(seg) => ir.bLInstrsBuffer += seg
        case None      => ()
      }
    }

  def returnIR()(implicit ir: IRBuilder): IR = {
    val strConsts = ir.strConstsBuffer.toList.zipWithIndex.map { case (str, i) =>
      CreateLabel(StrLabel(s"str$i", str))
    }
    val mainIRSeg = new IRSegment(strConsts, ir.instrsBuffer.toList)
    new IR(mainIRSeg :: ir.bLInstrsBuffer.toList)
  }

  def getNextStrId()(implicit ir: IRBuilder): Int =
    ir.strConstsBuffer.size

  def getBranchCounter()(implicit ir: IRBuilder): Int =
    ir.branchCounter

  def incBranchCounter()(implicit ir: IRBuilder) =
    ir.branchCounter += 1

  def findStrConstIndex(str: String)(implicit ir: IRBuilder): Int =
    ir.strConstsBuffer.indexOf(str)
}
