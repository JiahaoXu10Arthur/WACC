package wacc

object WACC_Builder {
  private def program(body: String) = s"""
    begin
      $body
    end
  """

  def buildExitProgram(expr: String): String = {
    program(s"""
      exit $expr
    """)
  }

  def buildSkipProgram(expr: String): String = {
  program(s"""
    skip
  """)
  }

  def buildPrintProgram(expr: String): String = {
    program(s"""
      print $expr
    """)
  }
}
