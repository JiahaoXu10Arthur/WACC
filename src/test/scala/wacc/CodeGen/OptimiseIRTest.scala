package wacc.CodeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import wacc.Instructions._
import wacc.CodeGen.OptimiseIR._

class OptimiseIRTest extends AnyFlatSpec {

    "Connect Move function" should "return None if at least one of the instructions is not a move instruction" in {
        val instr1 = Some(MovInstr(R0, R1))
        val instr2 = Some(AddInstr(R0, R1, R2))
        connectMove(instr1, instr2) should be(None)
    } 

    "Connect Move function" should "return the connected mov instruction if both instructions can be connected" in {
        val instr1 = Some(MovInstr(R0, R1))
        val instr2 = Some(MovInstr(R2, R0))
        connectMove(instr1, instr2) should be(Some(MovInstr(R2, R1)))
    } 

    "Connect Move function" should "return the connected mov instruction if the destination and src is not matched" in {
        val instr1 = Some(MovInstr(R0, R1))
        val instr2 = Some(MovInstr(R2, R1))
        connectMove(instr1, instr2) should be(None)
    } 

    "Optimise Push and Pop pairs" should "delete the push and pop instruction if the registers are the same" in {
        val instr1 = PushInstr(Seq(R0))
        val instr2 = PopInstr(Seq(R0))
        optimisePushPop(Seq(instr1, instr2)) should be(Seq())
    }

    "Optimise Push and Pop pairs" should "convert push and pop pairs to mov if the registers are not the same" in {
        val instr1 = PushInstr(Seq(R0))
        val instr2 = PopInstr(Seq(R1))
        optimisePushPop(Seq(instr1, instr2)) should be(Seq(MovInstr(R1, R0)))
    }

    "Optimise Push and Pop pairs" should "not change the instruction if the next instruction is not a pop instruction" in {
        val instr1 = PushInstr(Seq(R0))
        val instr2 = AddInstr(R0, R1, R2)
        optimisePushPop(Seq(instr1, instr2)) should be(Seq(instr1, instr2))
    }

    "Optimise Mov" should "combine moves with one's destination being the other's source" in {
        val instr1 = MovInstr(R0, R1)
        val instr2 = MovInstr(R2, R0)
        val instr3 = MovInstr(R3, R2)
        optimiseMov(Seq(instr1, instr2)) should be(Seq(MovInstr(R2, R1)))
        optimiseMov(Seq(instr1, instr2, instr3)) should be(Seq(MovInstr(R3, R1)))
    }

    "Optimise Mov" should "not change the instruction if the next instruction is not a mov instruction" in {
        val instr1 = MovInstr(R0, R1)
        val instr2 = AddInstr(R0, R1, R2)
        optimiseMov(Seq(instr1, instr2)) should be(Seq(instr1, instr2))
    }

    "Optimise Mov" should "not change if the next instruction's source and destination dosen't match'" in {
        val instr1 = MovInstr(R0, R1)
        val instr2 = MovInstr(R2, R5)
        optimiseMov(Seq(instr1, instr2)) should be(Seq(instr1, instr2))
    }

    "Optimise Mov" should "delete the current instruction if the source and destination are the same'" in {
        val instr1 = MovInstr(R0, R0)
        optimiseMov(Seq(instr1)) should be(Seq())
    }

    "Optimise Stode/Load pairs" should "replace load with move if the registers are the same" in {
        val opr1 = RegIntOffset(R1, 1)
        val instr1 = StoreInstr(R0, opr1, false)
        val instr2 = LoadInstr(R2, opr1, false)
        optimiseStoreLoad(Seq(instr1, instr2)) should be(Seq(instr1, MovInstr(R2, R0)))
    }
}