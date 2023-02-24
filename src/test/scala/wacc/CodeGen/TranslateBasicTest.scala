package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.WACC_Builder
import wacc.Instructions._

class TranslateBasicTest extends AnyFlatSpec {

  "Exit 0" should "be translated to exit" in pending
  // "Exit 0" should "be translated to exit" in {
  //   val (prog, st) = WACC_Builder.buildExitProgram("0")
  //   val IR = Translator.translate(prog, st)
  //   IR should matchPattern {
  //     case Seq(
  //       MovInstr(_, Immediate(0)), 
  //       PushInstr(_),
  //       PopInstr(Seq(R0)), 
  //       BranchLinkInstr(ExitLabel)) =>
  //   }
  // }

  "Exit -1" should "be translated to exit" in pending

  "Skip" should "be be ignored in translation" in pending

  // "Skip" should "be be ignored in translation" in {
  //   val (prog, st) = WACC_Builder.buildProgramWithBody(Seq("skip", "exit 0"))
  //   val IR = Translator.translate(prog, st)
  //   IR should matchPattern {
  //     case Seq(
  //       MovInstr(_, Immediate(0)), 
  //       PushInstr(_),
  //       PopInstr(Seq(R0)), 
  //       BranchLinkInstr(ExitLabel)) =>
  //   }
  // }
}
