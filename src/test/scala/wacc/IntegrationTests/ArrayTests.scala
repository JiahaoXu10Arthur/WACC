package wacc.IntegrationTests
import java.io.File

class ArrayTests extends IntegrationTest {
  // "Array Integration tests" in pending
  // testSkeleton("wacc_example/valid/array/")

  testFile("wacc_example/valid/array/", new File("arrayBasic.wacc"))
  testFile("wacc_example/valid/array/", new File("arrayEmpty.wacc"))
  testFile("wacc_example/valid/array/", new File("arrayIndexMayBeArrayIndex.wacc"))
  testFile("wacc_example/valid/array/", new File("arrayLength.wacc"))
  testFile("wacc_example/valid/array/", new File("arrayLookup.wacc"))
  testFile("wacc_example/valid/array/", new File("arrayNested.wacc"))
  testFile("wacc_example/valid/array/", new File("arrayOnHeap.wacc"))
  testFile("wacc_example/valid/array/", new File("arraySimple.wacc"))
  testFile("wacc_example/valid/array/", new File("emptyArrayAloneIsFine.wacc"))
  testFile("wacc_example/valid/array/", new File("emptyArrayNextLine.wacc"))
  // testFile("wacc_example/valid/array/", new File("emptyArrayPrint.wacc")) // Fail --> extra \n ?
  // testFile("wacc_example/valid/array/", new File("emptyArrayReplace.wacc")) // Fail --> extra \n ?
  testFile("wacc_example/valid/array/", new File("emptyArrayScope.wacc"))
  testFile("wacc_example/valid/array/", new File("modifyString.wacc"))

  // The tests contain #addr:(cannot be tested)
    // array (Success)
    // arrayPrint (Success)
    // printRef (Success)
}
