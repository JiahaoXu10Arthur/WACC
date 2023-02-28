package wacc.IntegrationTests

class IOTests extends IntegrationTest {
  testSkeleton("wacc_example/valid/IO/", skip = true)

  // Fail 9/20
  // /IOLoop --> no output
  // /print-backspace --> escape character?
  // /print --> extra /n?
}
