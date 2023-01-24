package wacc

import parsley.{Success, Failure}
import wacc.Rules.{expr}

class ExprParser {

  def expr_parse(arg: String): (Boolean, BigInt) = {
        expr.parse(arg) match {
            case Success(x) => return (true, x)
            case Failure(msg) => return (false, 0)
        }
    }
}
