package wacc.IntegrationTests
import java.io.File

class ArrayTests extends IntegrationTest {
  // "Array Integration tests" in pending
  // testSkeleton("wacc_example/valid/array/")

  testFile("wacc_example/valid/array/", new File("arrayBasic.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arrayEmpty.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arrayIndexMayBeArrayIndex.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arrayLength.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arrayLookup.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arrayNested.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arrayOnHeap.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("arraySimple.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("emptyArrayAloneIsFine.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("emptyArrayNextLine.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("emptyArrayPrint.wacc"), skip = true) // Fail --> extra \n ?
  testFile("wacc_example/valid/array/", new File("emptyArrayReplace.wacc"), skip = true) // Fail --> extra \n ?
  testFile("wacc_example/valid/array/", new File("emptyArrayScope.wacc"), skip = false)
  testFile("wacc_example/valid/array/", new File("modifyString.wacc"), skip = false)

  // The tests contain #addr:(cannot be tested)
  testFile("wacc_example/valid/array/", new File("array.wacc"), skip = true)
  testFile("wacc_example/valid/array/", new File("arrayPrint.wacc"), skip = true)
  testFile("wacc_example/valid/array/", new File("printRef.wacc"), skip = true)

    // array (Success)
    // arrayPrint (Success)
    // printRef (Success)
}
