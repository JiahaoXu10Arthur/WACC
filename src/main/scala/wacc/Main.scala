package wacc

import parsley.{Success, Failure}

object Main {
	def main(args: Array[String]): Unit = {
		Parser.parse(args.head) match {
			case Success(x) => {
				println(s"${args.head} parse success: ")
				println(x)
				System.exit(0)
			}
			case Failure(msg)	=> {
				println(s"${args.head} parse fail: ")
				println(msg)
				System.exit(100)
			}
		}
	}
}

