package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.Utils.WACC_Builder._ 
import wacc.Instructions._

class TranslateTest extends AnyFlatSpec {

  "Exit statements" should "contain correct exit instructions" in {
    val exitCode1 = 0
    val exitCode2 = 255
    val exitCode3 = -1
    val (prog1, st1) = buildExitProgram(exitCode1)
    val ir1 = Translator.translate(prog1, st1)
    val (prog2, st2) = buildExitProgram(exitCode2)
    val ir2 = Translator.translate(prog2, st2)
    val (prog3, st3) = buildExitProgram(exitCode3)
    val ir3 = Translator.translate(prog3, st3)
    ir1.instrs should contain (BranchLinkInstr(ExitLabel))
    ir2.instrs should contain (BranchLinkInstr(ExitLabel))
    ir3.instrs should contain (BranchLinkInstr(ExitLabel))
    /* IR instructions contains Mov / Ldr instructions to move constants */
    (ir1.instrs.foldLeft(false){(acc, instr) => acc || (instr match {
      case MovInstr(_, Immediate(exitCode1)) => true
      case _ => false
    })}) shouldBe true
    (ir2.instrs.foldLeft(false){(acc, instr) => acc || (instr match {
      case MovInstr(_, Immediate(exitCode2)) => true
      case _ => false
    })}) shouldBe true
    (ir3.instrs.foldLeft(false){(acc, instr) => acc || (instr match {
      case LoadInstr(_, Immediate(exitCode3), _) => true
      case _ => false
    })}) shouldBe true
  }

  "Print statements" should "contain correct print labels" in {
    val intExpr = "0"
    val charExpr = "\'f\'"
    val boolExpr = "true"
    val strExpr = "\"hello\""
    val (prog1, st1) = buildPrintProgram(intExpr)
    val ir1 = Translator.translate(prog1, st1)
    val (prog2, st2) = buildPrintProgram(charExpr)
    val ir2 = Translator.translate(prog2, st2)
    val (prog3, st3) = buildPrintProgram(boolExpr)
    val ir3 = Translator.translate(prog3, st3)
    val (prog4, st4) = buildPrintProgram(strExpr)
    val ir4 = Translator.translate(prog4, st4)
    val (prog5, st5) = buildPrintlnProgram(intExpr)
    val ir5 = Translator.translate(prog5, st5)
    ir1.bLInstrs.flatten should contain (CreateLabel(PrintInt))
    ir2.bLInstrs.flatten should contain (CreateLabel(PrintChar))
    ir3.bLInstrs.flatten should contain (CreateLabel(PrintBool))
    ir4.bLInstrs.flatten should contain (CreateLabel(PrintStr))
    ir5.bLInstrs.flatten should contain (CreateLabel(PrintLine))
  }

  "Arithmetic expressions" should "contain correct arithmetic instructions" in {
    val plusExpr = "1 + 2"
    val minusExpr = "1 - 2"
    val multExpr = "1 * 2"
    val divExpr = "1 / 2"
    val modExpr = "1 % 2"
    val complexExpr = "1 + 2 * 3 - 4"
    val (prog1, st1) = buildIntExprProgram(plusExpr)
    val ir1 = Translator.translate(prog1, st1)
    val (prog2, st2) = buildIntExprProgram(minusExpr)
    val ir2 = Translator.translate(prog2, st2)
    val (prog3, st3) = buildIntExprProgram(multExpr)
    val ir3 = Translator.translate(prog3, st3)
    val (prog4, st4) = buildIntExprProgram(divExpr)
    val ir4 = Translator.translate(prog4, st4)
    val (prog5, st5) = buildIntExprProgram(modExpr)
    val ir5 = Translator.translate(prog5, st5)
    val (prog6, st6) = buildIntExprProgram(complexExpr)
    val ir6 = Translator.translate(prog6, st6)
    ir1.instrs.map(x => x.getClass()) should contain (classOf[AddInstr])
    ir2.instrs.map(x => x.getClass()) should contain (classOf[SubInstr])
    ir3.instrs.map(x => x.getClass()) should contain (classOf[MulInstr])
    ir6.instrs.map(x => x.getClass()) should contain (classOf[AddInstr])
    ir6.instrs.map(x => x.getClass()) should contain (classOf[SubInstr])
    ir6.instrs.map(x => x.getClass()) should contain (classOf[MulInstr])
    ir4.instrs should contain (BranchLinkInstr(DivisionLabel))
    ir5.instrs should contain (BranchLinkInstr(DivisionLabel))
  }

  "Comparison expressions" should "contain correct comparison instructions" in {
    val eqExpr = "1 == 2"
    val neqExpr = "1 != 2"
    val ltExpr = "1 < 2"
    val gtExpr = "1 > 2"
    val leqExpr = "1 <= 2"
    val geqExpr = "1 >= 2"
    val (prog1, st1) = buildBoolExprProgram(eqExpr)
    val ir1 = Translator.translate(prog1, st1)
    val (prog2, st2) = buildBoolExprProgram(neqExpr)
    val ir2 = Translator.translate(prog2, st2)
    val (prog3, st3) = buildBoolExprProgram(ltExpr)
    val ir3 = Translator.translate(prog3, st3)
    val (prog4, st4) = buildBoolExprProgram(gtExpr)
    val ir4 = Translator.translate(prog4, st4)
    val (prog5, st5) = buildBoolExprProgram(leqExpr)
    val ir5 = Translator.translate(prog5, st5)
    val (prog6, st6) = buildBoolExprProgram(geqExpr)
    val ir6 = Translator.translate(prog6, st6)
    ir1.instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir2.instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir3.instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir4.instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir5.instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir6.instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
  }

  "Branch statements" should "contain jump labels" in {
    val (prog1, st1) = buildIfProgram("true", "print true", "print false")
    val (prog2, st2) = buildWhileProgram("true", "print true")
    val ir1 = Translator.translate(prog1, st1)
    (ir1.instrs.foldLeft(0){(acc, instr) => acc + (instr match {
      case CreateLabel(JumpLabel(_)) => 1
      case _ => 0
    })}) should be >= 2
    val ir2 = Translator.translate(prog2, st2)
    (ir2.instrs.foldLeft(0){(acc, instr) => acc + (instr match {
      case CondBranchInstr(_, JumpLabel(_)) => 1
      case _ => 0
    })}) should be >= 1
  }
}
