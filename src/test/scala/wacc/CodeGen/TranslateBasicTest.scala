package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.WACC_Builder
import wacc.Instructions._

class TranslateBasicTest extends AnyFlatSpec {
  "Exit 0" should "be translated to exit" in {
    val (prog, _) = WACC_Builder.buildExitProgram("0")
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(0)), 
        MovInstr(R0, _), 
        BranchLinkInstr(ExitLabel)) =>
    }
  }

  "Exit -1" should "be translated to exit" in pending

  "Skip" should "be be ignored in translation" in {
    val (prog, _) = WACC_Builder.buildProgramWithBody(Seq("skip", "exit 0"))
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(0)), 
        MovInstr(R0, _), 
        BranchLinkInstr(ExitLabel)) =>
    }
  }
}