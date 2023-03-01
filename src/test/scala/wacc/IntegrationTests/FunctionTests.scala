package wacc.IntegrationTests
import java.io.File

class FunctionTests extends IntegrationTest {
  //testSkeleton("wacc_example/valid/function/nested_functions/", skip = false)

  testFile("wacc_example/valid/function/nested_functions/", new File("fibonacciFullRec.wacc"), skip = false)
  testFile("wacc_example/valid/function/nested_functions/", new File("fibonacciRecursive.wacc"), skip = false)
  testFile("wacc_example/valid/function/nested_functions/", new File("fixedPointRealArithmetic.wacc"), skip = false)
  testFile("wacc_example/valid/function/nested_functions/", new File("functionConditionalReturn.wacc"), skip = false)
  testFile("wacc_example/valid/function/nested_functions/", new File("mutualRecursion.wacc"), skip = true) // Fail - stuck
  testFile("wacc_example/valid/function/nested_functions/", new File("printInputTriangle.wacc"), skip = true) // Fail - stuck
  testFile("wacc_example/valid/function/nested_functions/", new File("printTriangle.wacc"), skip = true) // Fail - stuck
  testFile("wacc_example/valid/function/nested_functions/", new File("simpleRecursion.wacc"), skip = false)

  testFile("wacc_example/valid/function/simple_functions/", new File("argScopeCanBeShadowed.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("asciiTable.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("functionDeclaration.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("functionDoubleReturn.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("functionIfReturns.wacc"), skip = false) 
  testFile("wacc_example/valid/function/simple_functions/", new File("functionManyArguments.wacc"), skip = true) // Fail
  testFile("wacc_example/valid/function/simple_functions/", new File("functionMultiReturns.wacc"), skip = false)

  testFile("wacc_example/valid/function/simple_functions/", new File("functionReturnPair.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("functionSimple.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("functionSimpleLoop.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("functionUpdateParameter.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("incFunction.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("manyArgumentsChar.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("manyArgumentsInt.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("negFunction.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("punning.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("sameArgName.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("sameArgName2.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("sameNameAsVar.wacc"), skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("usesArgumentWhilstMakingArgument.wacc"), skip = true) // Fail

}
