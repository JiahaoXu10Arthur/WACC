 package wacc
 import org.scalatest.flatspec.AnyFlatSpec
 import java.nio.file.Files
 import java.nio.file.Paths
 import java.io.File
 import parsley.{Success, Failure}

 import org.scalatest.matchers.should.Matchers._
 import org.scalatest.wordspec.AnyWordSpec
 
 class TempTest extends AnyWordSpec {
   def getListOfFiles(dir: String):List[File] = {
     val d = new File(dir)
     if (d.exists && d.isDirectory) {
       d.listFiles.filter(_.isFile).toList
     } else {
       List[File]()
     }
   }

   testSkeleton("wacc_example/valid/basic/exit/")

   def testSkeleton(path: String) = {
    val allFiles = getListOfFiles(path)

    "A bunch of generated ScalaTest tests" should {
        allFiles.foreach{x => testFile(path, x)}
    }
   }
   

   def testFile(path: String, f: File) = {
      val filename = path ++ f.getName()
      val string = new String(Files.readAllBytes(Paths.get(filename)))
      if (filename.contains("valid")) {
        "A successful compilation return the exit status 0 " ++ filename in {
           (Parser.parse(string) match {
               case Success(_) => true
               case Failure(_) => false
           }) shouldBe true
        }
      } else if (filename.contains("invalid")) {
        if (filename.contains("semanticErr")) {
            "A compilation that fails due to syntax errors return the exit status 100" in {
            }
        } else if (filename.contains("syntaxErr")) {
            "a compilation that fails due to semantic errors return the exit status 200" in {
            }
        }
      }
    }
   
 }
