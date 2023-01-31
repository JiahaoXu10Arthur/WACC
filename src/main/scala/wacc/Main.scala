package wacc

import parsley.{Success, Failure}

object Main {
	def main(args: Array[String]): Unit = {
		Parser.parse(args.head) match {
			case Success(x) => {
				println(s"${args.head} parse success: ")
				println(x)
			}
			case Failure(msg)	=> {
				println(s"${args.head} parse fail: ")
				println(msg)
			}
		}
	}
}

