package wacc

import parsley.{Success, Failure}
import wacc.Rules.{expr}

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_17!")

        expr.parse(args.head) match {
            case Success(x) => println(s"${args.head} = $x")
            case Failure(msg) => println(msg)
        }
    }
}

