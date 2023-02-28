package wacc.IntegrationTests
import java.io.File

class PairsTests extends IntegrationTest {
  // testSkeleton("wacc_example/valid/pairs/")

  testFile("wacc_example/valid/pairs/", new File("createPair.wacc"))
  testFile("wacc_example/valid/pairs/", new File("createPair02.wacc"))
  testFile("wacc_example/valid/pairs/", new File("createPair03.wacc"))
  testFile("wacc_example/valid/pairs/", new File("createRefPair.wacc"))
  testFile("wacc_example/valid/pairs/", new File("free.wacc"))
  testFile("wacc_example/valid/pairs/", new File("linkedList.wacc"))
  testFile("wacc_example/valid/pairs/", new File("nestedPair.wacc"))
  testFile("wacc_example/valid/pairs/", new File("nestedPairLeftAssign.wacc"))
  testFile("wacc_example/valid/pairs/", new File("nestedPairRightExtract.wacc"))
  testFile("wacc_example/valid/pairs/", new File("null.wacc"))
  // testFile("wacc_example/valid/pairs/", new File("pairarray.wacc")) // Fail --> extra \n ?
  testFile("wacc_example/valid/pairs/", new File("printNull.wacc"))
  testFile("wacc_example/valid/pairs/", new File("printNullPair.wacc"))
  // testFile("wacc_example/valid/pairs/", new File("readPair.wacc")) // Fail --> extra \n ?
  testFile("wacc_example/valid/pairs/", new File("writeFst.wacc"))
  testFile("wacc_example/valid/pairs/", new File("writeSnd.wacc"))

  // The tests contain #addr:(cannot be tested)
    // checkRefPair (Success)
    // printPair (Succeess)
    // printPairOfNulls (Success)
}
