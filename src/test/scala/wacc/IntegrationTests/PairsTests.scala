package wacc.IntegrationTests

import java.io.File

class PairsTests extends IntegrationTest {
  // testSkeleton("wacc_example/valid/pairs/")

  testFile("wacc_example/valid/pairs/", new File("createPair.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("createPair02.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("createPair03.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("createRefPair.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("free.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("linkedList.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("nestedPair.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("nestedPairLeftAssign.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("nestedPairRightExtract.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("null.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("pairarray.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("printNull.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("printNullPair.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("readPair.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("writeFst.wacc"), skip = false)
  testFile("wacc_example/valid/pairs/", new File("writeSnd.wacc"), skip = false)

  // The tests contain #addr:
  testFile("wacc_example/valid/pairs/", new File("checkRefPair.wacc"), skip = true)
  testFile("wacc_example/valid/pairs/", new File("printPair.wacc"), skip = true)
  testFile("wacc_example/valid/pairs/", new File("printPairOfNulls.wacc"), skip = true)
}
