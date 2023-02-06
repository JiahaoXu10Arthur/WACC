 package wacc

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

  def getListOfSubDirectories(directoryName: String): List[String] = {
    (new File(directoryName))
        .listFiles
        .filter(_.isDirectory)
        .map(_.getName).toList
  }

  var testNum = 0
  var testRun = 0
  var testPass = 0
  var testFail = 0
  var testSkip = 0

  println("Running all tests")
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
      testNum += 1
      testRun += 1
      val filename = path ++ f.getName()
      val string = new String(Files.readAllBytes(Paths.get(filename)))

      if (path.contains("/valid/")) {
          "A successful compilation return the exit status 0 " ++ filename in {
           (Parser.parse(string) match {
               case Success(x) => {
                // println(x)
                testPass += 1
                true
               }
               case Failure(msg) => {
                println(msg)
                testFail += 1
                false
               }
           }) shouldBe true
        }
      } else if (path.contains("syntaxErr")) {
<<<<<<< HEAD
        "A compilation that fails due to syntax errors return the exit status 100" ++ filename in pending
        
        // {
        //   (Parser.parse(string) match {
        //       case Success(x) => {
        //       // println(x)
        //       testFail += 1
        //       true
        //       }
        //       case Failure(msg) => {
        //       println(msg)
        //       testPass += 1
        //       false
        //       }
        //   }) shouldBe false
        // }


=======
        testSkip += 1
        "A compilation that fails due to syntax errors return the exit status 100" ++ filename in {
           (Parser.parse(string) match {
               case Success(x) => {
                testFail += 1
                false
               }
               case Failure(msg) => {
                println(msg)
                testPass += 1
                true
               }
           }) shouldBe true
        }
>>>>>>> 7e7062fe6637fd3709fd84dc64416edd73d55b7f
      } else if (path.contains("semanticErr")) {
        testSkip += 1
        "A compilation that fails due to semantic errors return the exit status 200" ++ filename in pending
      }
    }



   
 }
