package wacc

import parsley.{Success, Failure}
import wacc.Rules.{expr}

object Main {
	def main(args: Array[String]): Unit = {
		println("Hello WACC_17!")

		// lazy val integer = digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)

		// val add = (x: BigInt, y: BigInt) => x + y
		// val sub = (x: BigInt, y: BigInt) => x - y

		// lazy val expr: Parsley[BigInt] =
		//     chain.left1[BigInt](
		//         ('(' ~> expr <~ ')') <|> integer,
		//         ('+' #> add) <|> ('-' #> sub)
		//     )

		// expr.parse(args.head) match {
		//     case Success(x) => println(s"${args.head} = $x")
		//     case Failure(msg) => println(msg)
		// }

		Parser.exprParse(args.head) match {
				case true => println(s"${args.head} parse success")
				case false => println(s"${args.head} parse fail")
		}


	}
}

