// package wacc

// import org.scalatest.flatspec.AnyFlatSpec
// import java.nio.file.Files
// import java.nio.file.Paths

// class TempTest extends AnyFlatSpec {
//   val filename = "wacc_examples/valid/basic/exit/exitBasic.wacc"

//   val string = new String(Files.readAllBytes(Paths.get(filename)))

//   if (filename.contains("valid")) {

//     "A successful compilation " should ("return the exit status 0") in {

//     }

//   } else if (filename.contains("invalid")) {
//     if (filename.contains("semanticErr")) {
//         "A compilation that fails due to syntax errors" should ("return the exit status 100") in {

//         }
//     } else if (filename.contains("syntaxErr")) {
//         "a compilation that fails due to semantic errors" should ("return the exit status 200") in {

//         }
//     }
//   }
// }
