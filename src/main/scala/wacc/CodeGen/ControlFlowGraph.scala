package wacc.CodeGen

import wacc.SemanticChecker.ImmutableSymbolTable
import wacc.Ast._
import ControlFlowNodes._
import wacc.Instructions._


object ControlFlowGraph {
  def genControlFlowGraph(program: Program, st: ImmutableSymbolTable): ControlFlowNode = {
    InstrNode(SkipInstr(), null)
  }

}
