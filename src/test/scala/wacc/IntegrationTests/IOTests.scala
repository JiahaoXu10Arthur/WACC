package wacc.IntegrationTests

class IOTests extends IntegrationTest {
  testSkeleton("wacc_example/valid/IO/")

  // Fail 9/20
  // /IOLoop --> no output
  // /multipleStringsAssignment --> no output
  // /print-backspace --> escape character?
  // /print --> extra /n?
}
