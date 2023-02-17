 package wacc

 import java.nio.file.Files
 import java.nio.file.Paths
 import java.io.File
 import parsley.{Success, Failure}

 import org.scalatest.matchers.should.Matchers._
 import org.scalatest.wordspec.AnyWordSpec
 import wacc.SyntaxChecker.Parser
 import wacc.SemanticChecker.SemanticChecker
 
 class FrontIntegrationTest extends AnyWordSpec {

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

  println("Running all tests")
  // testFile("wacc_example/valid/pairs/", new File("nestedPairRightExtract.wacc"))
  testSkeleton("wacc_example/")

  def testSkeleton(path: String) :Unit = {
    val allFiles = getListOfFiles(path)
    val subDirs = getListOfSubDirectories(path)

    "A bunch of generated ScalaTest tests" should {
      allFiles.foreach{x => testFile(path, x)}
      if (subDirs.nonEmpty) {
        subDirs.foreach{x => testSkeleton(path ++ "/" ++ x ++ "/")}
      }
    }
  }

   def testFile(path: String, f: File)= {
      val filename = path ++ f.getName()
      val string = new String(Files.readAllBytes(Paths.get(filename)))

      if (path.contains("/valid/")) {
          "A successful compilation return the exit status 0 " ++ filename in {
           (Parser.parse(string) match {
               case Success(x) => {
                SemanticChecker.semanticCheck(x)._1 shouldBe empty
                true
               }
               case Failure(msg) => {
                false
               }
           }) shouldBe true
        }
      } else if (path.contains("syntaxErr")) {
        "A compilation that fails due to syntax errors return the exit status 100" ++ filename in {
           (Parser.parse(string) match {
               case Success(x) => {
                false
               }
               case Failure(msg) => {
                true
               }
           }) shouldBe true
        }
      } else if (path.contains("semanticErr")) {
        "A compilation that fails due to semantic errors return the exit status 200" ++ filename in {
           (Parser.parse(string) match {
               case Success(x) => {
                SemanticChecker.semanticCheck(x)._1 should not be empty
                true
               }
               case Failure(msg) => {
                false
               }
           }) shouldBe true
        }
      }
  }
   
 }
