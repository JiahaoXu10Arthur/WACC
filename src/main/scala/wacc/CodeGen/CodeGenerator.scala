package wacc.CodeGen

import wacc.Ast._
import wacc.SemanticChecker.SymbolTable

object CodeGenerator {
  def generateAssembly(ast: Program, st: SymbolTable, waccName: String): String = {
    val ir = Translator.translate(ast, st)
    ArmAsmGenerator.assemble(ir, waccName)
  }
}
