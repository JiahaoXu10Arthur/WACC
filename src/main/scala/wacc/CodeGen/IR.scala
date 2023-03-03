package wacc.CodeGen

import wacc.Instructions._

class IRSegment(_literals: Seq[Instruction], _instrs: Seq[Instruction]) {
  val literals: Seq[Instruction] = DataTag +: _literals
  val instrs: Seq[Instruction]   = TextTag +: _instrs
}

class IR(val segments: Seq[IRSegment])
