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
    ir1.segments should not be empty
    ir2.segments should not be empty
    ir3.segments should not be empty
    val ir1Instrs = ir1.segments.head.instrs
    val ir2Instrs = ir2.segments.head.instrs
    val ir3Instrs = ir3.segments.head.instrs

    /* IR instructions contains exit instructions*/
    ir1Instrs should contain (BranchLinkInstr(ExitLabel))
    ir2Instrs should contain (BranchLinkInstr(ExitLabel))
    ir3Instrs should contain (BranchLinkInstr(ExitLabel))
    /* IR instructions contains Mov / Ldr instructions to move constants */
    (ir1Instrs.foldLeft(false){(acc, instr) => acc || (instr match {
      case MovInstr(_, Immediate(exitCode1)) => true
      case _ => false
    })}) shouldBe true
    (ir2Instrs.foldLeft(false){(acc, instr) => acc || (instr match {
      case MovInstr(_, Immediate(exitCode2)) => true
      case _ => false
    })}) shouldBe true
    (ir3Instrs.foldLeft(false){(acc, instr) => acc || (instr match {
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
    ir1.segments should not be empty
    ir2.segments should not be empty
    ir3.segments should not be empty
    ir4.segments should not be empty
    ir5.segments should not be empty
    val ir1Instrs = ir1.segments.map{x => x.instrs}.flatten
    val ir2Instrs = ir2.segments.map{x => x.instrs}.flatten
    val ir3Instrs = ir3.segments.map{x => x.instrs}.flatten
    val ir4Instrs = ir4.segments.map{x => x.instrs}.flatten
    val ir5Instrs = ir5.segments.map{x => x.instrs}.flatten


    /* IR instructions contains print instructions*/
    ir1Instrs should contain (CreateLabel(PrintInt))
    ir2Instrs should contain (CreateLabel(PrintChar))
    ir3Instrs should contain (CreateLabel(PrintBool))
    ir4Instrs should contain (CreateLabel(PrintStr))
    ir5Instrs should contain (CreateLabel(PrintLine))
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
    ir1.segments should not be empty
    ir2.segments should not be empty
    ir3.segments should not be empty
    ir4.segments should not be empty
    ir5.segments should not be empty
    ir6.segments should not be empty
    val ir1Instrs = ir1.segments.map{x => x.instrs}.flatten
    val ir2Instrs = ir2.segments.map{x => x.instrs}.flatten
    val ir3Instrs = ir3.segments.map{x => x.instrs}.flatten
    val ir4Instrs = ir4.segments.map{x => x.instrs}.flatten
    val ir5Instrs = ir5.segments.map{x => x.instrs}.flatten
    val ir6Instrs = ir6.segments.map{x => x.instrs}.flatten
    ir1Instrs.map(x => x.getClass()) should contain (classOf[AddInstr])
    ir2Instrs.map(x => x.getClass()) should contain (classOf[SubInstr])
    ir3Instrs.map(x => x.getClass()) should contain (classOf[MulInstr])
    ir6Instrs.map(x => x.getClass()) should contain (classOf[AddInstr])
    ir6Instrs.map(x => x.getClass()) should contain (classOf[SubInstr])
    ir6Instrs.map(x => x.getClass()) should contain (classOf[MulInstr])
    ir4Instrs should contain (BranchLinkInstr(DivisionLabel))
    ir5Instrs should contain (BranchLinkInstr(DivisionLabel))
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
    ir1.segments should not be empty
    ir2.segments should not be empty
    ir3.segments should not be empty
    ir4.segments should not be empty
    ir5.segments should not be empty
    ir6.segments should not be empty
    val ir1Instrs = ir1.segments.map{x => x.instrs}.flatten
    val ir2Instrs = ir2.segments.map{x => x.instrs}.flatten
    val ir3Instrs = ir3.segments.map{x => x.instrs}.flatten
    val ir4Instrs = ir4.segments.map{x => x.instrs}.flatten
    val ir5Instrs = ir5.segments.map{x => x.instrs}.flatten
    val ir6Instrs = ir6.segments.map{x => x.instrs}.flatten
    ir1Instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir2Instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir3Instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir4Instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir5Instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
    ir6Instrs.map(x => x.getClass()) should contain (classOf[CmpInstr])
  }

  "Branch statements" should "contain jump labels" in {
    val (prog1, st1) = buildIfProgram("true", "print true", "print false")
    val (prog2, st2) = buildWhileProgram("true", "print true")
    val ir1 = Translator.translate(prog1, st1)
    ir1.segments should not be empty
    val ir1Instrs = ir1.segments.map{x => x.instrs}.flatten
    (ir1Instrs.foldLeft(0){(acc, instr) => acc + (instr match {
      case CreateLabel(JumpLabel(_)) => 1
      case _ => 0
    })}) should be >= 2
    val ir2 = Translator.translate(prog2, st2)
    ir2.segments should not be empty
    val ir2Instrs = ir2.segments.map{x => x.instrs}.flatten
    (ir2Instrs.foldLeft(0){(acc, instr) => acc + (instr match {
      case CondBranchInstr(_, JumpLabel(_)) => 1
      case _ => 0
    })}) should be >= 1
  }
}
