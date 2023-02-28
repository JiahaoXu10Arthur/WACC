package wacc.IntegrationTests
import java.io.File

class FunctionTests extends IntegrationTest {
  // testSkeleton("wacc_example/valid/function/simple_functions/", skip = false)
  testFile("wacc_example/valid/function/simple_functions/", new File("asciiTable.wacc"), skip = false)


}
