package wacc.Utils
import java.io._
import scala.sys.process._
import scala.io._
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

object BackEndUtils {

    private final val WACCLENGTH = 5
    private final val INPUTLENGTH = 9

    class Expects(val _input: String, _output: String, _exit: String) {
        def input: String = _input
        def output: String = _output
        def exit: String = _exit
    }

    def runCommand(cmd: ProcessBuilder): (Int, String) = {
      val stdoutStream = new ByteArrayOutputStream
      val exitValue = (cmd #> stdoutStream).!
      (exitValue, stdoutStream.toString())
    }

    def getExpectStrings(filename: String): Expects = {

        val lines = Source.fromFile(filename).getLines().toArray

        val inputBuff, outputBuff, exitBuff = new ListBuffer[String]()
        var inputFlag, outputFlag, exitFlag = false

        breakable {
            for (l<-lines) {
                if (l.contains("Input:"))
                    inputFlag = true
                else if (l.contains("Output:")) {
                    inputFlag = false
                    outputFlag = true
                }
                else if (l.contains("Exit:")) {
                    inputFlag = false
                    outputFlag = false
                    exitFlag = true
                }
                else if (l.contains("Program:")) {
                    break()
                }
            
                if (inputFlag)
                    inputBuff += l
                else if (outputFlag)
                    outputBuff += l
                else if (exitFlag)
                    exitBuff += l
            }
        }
        
        new Expects(inputBuff.mkString.drop(INPUTLENGTH), cutString(outputBuff), cutString(exitBuff))
    }

    def cutString(buff: ListBuffer[String]): String = {
        val ignorable = "#"
        val ignorable2 = " "
        val empty = ""
        buff.drop(1).dropRight(1).map(_.replaceFirst(ignorable, empty).replaceFirst(ignorable2, empty)).mkString("\n")
    }

    def getExpects(filename: String): (String, String) = {
        val expects = getExpectStrings(filename ++ ".wacc")

        val output = expects.output
        var exit = expects.exit

        if (exit.isEmpty()) {
            exit = "0"
        }

        (output, exit)
    }

    def getOutputAndExit(filename: String): (String, String) = {
        val input = getExpectStrings(filename ++ ".wacc").input

        ("arm-linux-gnueabi-gcc -o " ++ filename ++ " -mcpu=arm1176jzf-s -mtune=arm1176jzf-s " ++ filename ++ ".s").!
        val inputStream = new ByteArrayInputStream(input.getBytes())
        val a = Seq(
            "qemu-arm" ,"-L", "/usr/arm-linux-gnueabi/", filename
        ) #< inputStream
        val res@(exitCode, output) = runCommand(a)

        /* Removes generated executable */
        s"rm $filename".!

        (output, exitCode.toString())
    }

    def replaceAddrs(_output: String, expect: String): String = {
        val addrs = "#addrs#"
        val addrsRegex = "0x[a-f0-9]{5}".r

        val runtimeErr = "#runtime_error#"
        val fatalErr = "fatal error:"

        var output = _output
        if (expect.contains(addrs))
            output = addrsRegex.replaceAllIn(output, addrs)

        if (expect.contains(runtimeErr)) {
            val start = output.indexOf(fatalErr)
            if (start == -1) println(output)
            val sub = output.substring(start)
            output = output.replace(sub, runtimeErr)
        }    
        output
    }
}

