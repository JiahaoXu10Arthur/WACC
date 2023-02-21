package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.WACC_Builder
import wacc.Instructions._

class TranslatePrintTest extends AnyFlatSpec {
  "Print \'f\'" should "translate to print char" in {
    val (prog, _) = WACC_Builder.buildPrintProgram("\'f\'")
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(102)), 
        PushInstr(_),
        PopInstr(Seq(R0)), 
        BranchLinkInstr(PrintChar)) =>
    }
  }

  "Println \'f\'" should "translate to println char" in {
    val (prog, _) = WACC_Builder.buildPrintlnProgram("\'f\'")
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(102)), 
        PushInstr(_),
        PopInstr(Seq(R0)), 
        BranchLinkInstr(PrintChar),
        BranchLinkInstr(PrintLine)) =>
    }
  }

  "Print 123" should "translate to print int" in {
    val (prog, _) = WACC_Builder.buildPrintProgram("123")
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(123)), 
        PushInstr(_),
        PopInstr(Seq(R0)), 
        BranchLinkInstr(PrintInt)) =>
    }
  }

  "Print true" should "translate to print bool" in {
    val (prog, _) = WACC_Builder.buildPrintProgram("true")
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(1)), 
        PushInstr(_),
        PopInstr(Seq(R0)), 
        BranchLinkInstr(PrintBool)) =>
    }
  }

  "Print false" should "translate to print bool" in {
    val (prog, _) = WACC_Builder.buildPrintProgram("false")
    val IR = Translator.translate(prog)
    IR should matchPattern {
      case Seq(
        MovInstr(_, Immediate(0)), 
        PushInstr(_),
        PopInstr(Seq(R0)), 
        BranchLinkInstr(PrintBool)) =>
    }
  }
}
