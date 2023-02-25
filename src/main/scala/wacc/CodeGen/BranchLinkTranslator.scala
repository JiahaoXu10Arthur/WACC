package wacc.CodeGen

import scala.collection.mutable
import wacc.Instructions._

object BranchLinkTranslator {

	def translateBranchLink(blName: BranchLinkName): List[Instruction] = {
		implicit val instrsBuffer = mutable.ListBuffer[Instruction]()

		blName match {
			case ArrayLoad => 
			case ArrayStore =>
			case ArrayStoreB => 
			case CheckBound => 
			case CheckDivZero => 
			case CheckNull => 
			case CheckOverflow => 
			case DivisionLabel => 
			case ExitLabel => // Default
			case FreeLabel => // Default
			case FreePair => 
			case MallocLabel => // Default
			case PrintBool => 
			case PrintChar => 
			case PrintInt => 
			case PrintLine => 
			case PrintPointer => 
			case PrintStr => 
			case ReadChar => 
			case ReadInt => 
		}

		instrsBuffer.toList
	}


}
