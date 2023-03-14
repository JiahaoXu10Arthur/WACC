package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

object CodeGenerator {
  def generateAssembly(ast: Program, st: SymbolTable, waccName: String, peephole: Boolean, tailRec: Boolean): String = {
    val ir = Translator.translate(ast, st, tailRec)
    val finalIR = (peephole match {
      case true  => OptimiseIR.makeOptimisedIR(ir)
      case false => ir
    })
    ArmAsmGenerator.assemble(finalIR, waccName)
  }
}
