package wacc
import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import java.util.Scanner
import java.io.File

class ParserExampleExprTest extends AnyFlatSpec {
    val testDir = "../wacc_examples/valid/expressions"

    def getListOfFiles(dir: File):List[File] = dir.listFiles.filter(_.isFile).toList
    val allFiles = getListOfFiles(new File(testDir))

    for (i <- allFiles) {
        println("File name: " + i)
        val lines = scala.io.Source.fromFile(i).mkString
        println(lines)
    }
}
