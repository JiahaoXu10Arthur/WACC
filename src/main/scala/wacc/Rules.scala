package wacc

import parsley.{Parsley}
import parsley.character.digit
import parsley.expr.chain
import parsley.implicits.character.charLift

object Rules {
    lazy val integer = digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)

    val add = (x: BigInt, y: BigInt) => x + y
    val sub = (x: BigInt, y: BigInt) => x - y

    lazy val expr: Parsley[BigInt] =
        chain.left1[BigInt](
            ('(' ~> expr <~ ')') <|> integer,
            ('+' #> add) <|> ('-' #> sub)
    )
}
