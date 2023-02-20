import java.io._
import scala.sys.process._
import scala.io._
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

object Test {

    private final val WACCLENGTH = 5
    private final val INPUTLENGTH = 9

    class Expects(val _input: String, _output: String, _exit: String) {
        def input: String = _input
        def output: String = _output
        def exit: String = _exit
    }

    def runCommand(cmd: ProcessBuilder): (Int, String, String) = {
      val stdoutStream = new ByteArrayOutputStream
      val stderrStream = new ByteArrayOutputStream
      val stdoutWriter = new PrintWriter(stdoutStream)
      val stderrWriter = new PrintWriter(stderrStream)
      val exitValue = cmd.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
      stdoutWriter.close()
      stderrWriter.close()
      (exitValue, stdoutStream.toString, stderrStream.toString)
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
        val ignorable = " #"
        buff.drop(1).dropRight(1).map(_.dropWhile(a => ignorable.indexOf(s"$a") >= 0)).mkString("\n")
    }

    def getExpects(filename: String): (String, String) = {
        val expects = getExpectStrings(filename)

        (expects.output, expects.exit)
    }

    def getOutputAndExit(filename: String): (Int, String) = {
        val input = getExpectStrings(filename).input

        val name = filename.dropRight(WACCLENGTH)

        ("arm-linux-gnueabi-gcc -o EXEName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s" ++ name ++ ".s").!
        val inputStream = new ByteArrayInputStream(input.getBytes())
        val a = Seq(
            "qemu-arm" ,"-L", "/usr/arm-linux-gnueabi/", name
        ) #< inputStream
        val res@(exitCode, output, _) = runCommand(a)

        (exitCode, output)
    }

     def main(args: Array[String]): Unit = {
        val filename = "echoChar.wacc"
        val (_output, _exit) = getExpects(filename)
        val (output, exit) = getOutputAndExit(filename)

        println(getExpects(filename))
        println(getOutputAndExit(filename))
     }
}

