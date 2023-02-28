package wacc.IntegrationTests

 import java.nio.file.Files
 import java.nio.file.Paths
 import java.io.File
 import parsley.{Success, Failure}

 import org.scalatest.matchers.should.Matchers._
 import org.scalatest.wordspec.AnyWordSpec
 import wacc.SyntaxChecker.Parser
 import wacc.SemanticChecker.SemanticChecker
 import wacc.CodeGen.Translator
 import wacc.CodeGen.CodeGenerator
 import wacc.Utils.BackEndUtils._
 
 class IntegrationTest extends AnyWordSpec {

  private final val WACC_FILE_DROP_LEN = 5

   def getListOfFiles(dir: String):List[File] = {
     val d = new File(dir)
     if (d.exists && d.isDirectory) {
       d.listFiles.filter(_.isFile).toList
     } else {
       List[File]()
     }
   }

  def getListOfSubDirectories(directoryName: String): List[String] = {
    (new File(directoryName))
        .listFiles
        .filter(_.isDirectory)
        .map(_.getName).toList
  }

  // println("Running all tests")
  // testFile("wacc_example/valid/pairs/", new File("nestedPairRightExtract.wacc"))
  // testSkeleton("wacc_example/")

  def testSkeleton(path: String, skip: Boolean = false) :Unit = {
    val allFiles = getListOfFiles(path)
    val subDirs = getListOfSubDirectories(path)
    allFiles.foreach{x => if (x.getName().endsWith(".wacc")) testFile(path, x, skip)}
    if (subDirs.nonEmpty) {
      subDirs.foreach{x => testSkeleton(s"$path/$x/", skip)}
    }
  }

   def testFile(path: String, f: File, skip: Boolean)= {
      val filename = path ++ f.getName()
      val string = new String(Files.readAllBytes(Paths.get(filename)))
      if (path.contains("/valid/")) {
          s"Compiling valid file ${f.getName()} should succeed" in {
            skip match {
              case true => {
                println("Skipping " ++ "filename")
                pending
              }
              case false => (
                Parser.parse(string) match {
                  case Success(x) => {
                    val (errors, st) = SemanticChecker.semanticCheck(x)
                    errors shouldBe empty
                    val ir = Translator.translate(x, st)
                    val waccName = filename.dropRight(WACC_FILE_DROP_LEN)
                    CodeGenerator.assemble(ir, waccName)
                    val (_output, _exit) = getExpects(waccName)
                    val (output, exit) = getOutputAndExit(waccName)
                    compareOutput(output, _output) shouldBe true 
                    exit shouldBe _exit
                    true
                  }
                  case Failure(msg) => false
                }) shouldBe true
            }
        }
      } else if (path.contains("syntaxErr")) {
        s"Compiling invalid file ${f.getName()} with syntax error should fail" in {
          skip match {
            case true => pending
            case false => (
              Parser.parse(string) match {
                case Success(x) => {
                  false
                }
                case Failure(msg) => {
                  true
                }
              }) shouldBe true
          }
        }
      } else if (path.contains("semanticErr")) {
        s"Compiling invalid file ${f.getName()} with semantic error should fail" in {
          skip match {
            case true => pending
            case false => (
              Parser.parse(string) match {
                case Success(x) => {
                  val (errors, st) = SemanticChecker.semanticCheck(x)
                  !errors.isEmpty
                }
                case Failure(msg) => {
                  false
                }
              }) shouldBe true
          }
        }
      }
  }
   
 }
