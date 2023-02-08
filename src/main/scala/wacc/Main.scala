package wacc

import parsley.{Success, Failure}
import java.nio.file.Files
import java.nio.file.Paths
import Errors.errorsMkString

object Main {
  def main(args: Array[String]): Unit = {
    val filename = args.head
    val string = new String(Files.readAllBytes(Paths.get(filename)))

    Parser.parse(string) match {
      case Success(x) => {
        // println(x)
        val errors = SemanticChecker.semanticCheck(x)
        if (errors.isEmpty) {
          println(s"${args.head} parse success")
          System.exit(0)
        } else {
          println(s"${args.head} parse fail: ")
          println(errorsMkString(errors, filename))
          System.exit(200)
        }
      }
      case Failure(err) => {
        println(s"${args.head} parse fail: ")
        println(errorsMkString(Seq(err), filename))
        System.exit(100)
      }
    }

  }
}
