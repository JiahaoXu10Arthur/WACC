package wacc

object Ast {

  sealed trait Expr
    case class IntLit(value: Int) extends Expr
    case class BoolLit(value: Boolean) extends Expr
    case class CharLit(value: Char) extends Expr
    case class StrLit(value: String) extends Expr
    case class PairLit(value: String) extends Expr
    case class Ident(name: String) extends Expr
    case class ArrayElem(array: Ident, index: Expr) extends Expr
    case class UnOp(op: UnaryOper, expr: Expr) extends Expr
    case class BinOp(op: BinaryOper, expr1: Expr, expr2: Expr) extends Expr
    case class Parens(expr: Expr) extends Expr

  sealed trait UnaryOper
    case class Not(expr: Expr) extends Expr
    case class Neg(expr: Expr) extends UnaryOper
    case class Len(expr: Expr) extends UnaryOper
    case class Ord(expr: Expr) extends UnaryOper
    case class Chr(expr: Expr) extends UnaryOper
  
  sealed trait BinaryOper
    /* Arithmetic binary operators */
    case class Mul(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Div(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Mod(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Add(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Sub(expr1: Expr, expr2: Expr) extends BinaryOper
    /* Comparison binary operators */
    case class Gt(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Gte(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Lt(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Lte(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Eq(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Neq(expr1: Expr, expr2: Expr) extends BinaryOper
    /* Logical binary operators */
    case class And(expr1: Expr, expr2: Expr) extends BinaryOper
    case class Or(expr1: Expr, expr2: Expr) extends BinaryOper
}