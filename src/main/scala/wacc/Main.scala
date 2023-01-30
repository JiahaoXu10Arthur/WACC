package wacc

import parsley.{Parsley, Success, Failure}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift
import StatParser.{statParse}

object Main {
	def main(args: Array[String]): Unit = {
		// Parser.parse(args.head) match {
		// 	case Some(_) => println(s"${args.head} parse success")
		// 	case None	 => println(s"${args.head} parse fail")
		// }
		StatParser.statParse(args.head) match {
			case Some(_) => println(s"${args.head} parse success")
			case None	 => println(s"${args.head} parse fail")
		}
	}
}

