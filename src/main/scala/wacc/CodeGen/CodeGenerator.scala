package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

object CodeGenerator {
  def generateAssembly(ast: Program, st: SymbolTable, waccName: String): String = {
    val ir = Translator.translate(ast, st)
    val ir_op1 = OptimiseIR.makeOptimisedIR(ir)
    ArmAsmGenerator.assemble(ir_op1, waccName)
  }
}
