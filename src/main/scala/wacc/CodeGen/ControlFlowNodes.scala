package wacc.CodeGen

import wacc.Instructions._

object ControlFlowNodes {
  sealed trait ControlFlowNode

  case class InstrNode(instr: Instruction, next: ControlFlowNode) extends ControlFlowNode

  case class BranchNode(instr: Instruction, trueNode: ControlFlowNode, falseNode: ControlFlowNode)
      extends ControlFlowNode
}
