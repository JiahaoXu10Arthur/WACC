package wacc

import parsley.genericbridges.{ParserBridge1, ParserBridge2, ParserBridge3, ParserBridge4}
import Types._

object Ast {

  /* Binary Expressions */
  sealed trait Expr extends Rvalue
    /* Arithmetic binary operators */
    case class Mul(expr1: Expr, expr2: Expr) extends Expr
    object Mul extends ParserBridge2[Expr, Expr, Mul]

    case class Div(expr1: Expr, expr2: Expr) extends Expr
    object Div extends ParserBridge2[Expr, Expr, Div]

    case class Mod(expr1: Expr, expr2: Expr) extends Expr
    object Mod extends ParserBridge2[Expr, Expr, Mod]
    
    case class Add(expr1: Expr, expr2: Expr) extends Expr
    object Add extends ParserBridge2[Expr, Expr, Add]
    
    case class Sub(expr1: Expr, expr2: Expr) extends Expr
    object Sub extends ParserBridge2[Expr, Expr, Sub]
    
    /* Comparison binary operators */
    case class Gt(expr1: Expr, expr2: Expr) extends Expr
    object Gt extends ParserBridge2[Expr, Expr, Gt]
    
    case class Gte(expr1: Expr, expr2: Expr) extends Expr
    object Gte extends ParserBridge2[Expr, Expr, Gte]
    
    case class Lt(expr1: Expr, expr2: Expr) extends Expr
    object Lt extends ParserBridge2[Expr, Expr, Lt]
    
    case class Lte(expr1: Expr, expr2: Expr) extends Expr
    object Lte extends ParserBridge2[Expr, Expr, Lte]

    case class Eq(expr1: Expr, expr2: Expr) extends Expr
    object Eq extends ParserBridge2[Expr, Expr, Expr]

    case class Neq(expr1: Expr, expr2: Expr) extends Expr
    object Neq extends ParserBridge2[Expr, Expr, Neq]

    /* Logical binary operators */
    case class And(expr1: Expr, expr2: Expr) extends Expr
    object And extends ParserBridge2[Expr, Expr, And]
    
    case class Or(expr1: Expr, expr2: Expr) extends Expr
    object Or extends ParserBridge2[Expr, Expr, Or]

  /* Unary operators */
  sealed trait Term extends Expr
    case class Not(expr: Expr) extends Expr
    object Not extends ParserBridge1[Expr, Not]

    case class Neg(expr: Expr) extends Expr
    object Neg extends ParserBridge1[Expr, Neg]

    case class Len(expr: Expr) extends Expr
    object Len extends ParserBridge1[Expr, Len]

    case class Ord(expr: Expr) extends Expr
    object Ord extends ParserBridge1[Expr, Ord]

    case class Chr(expr: Expr) extends Expr
    object Chr extends ParserBridge1[Expr, Chr]

  /* Literals */
  sealed trait Atom extends Term    
    case class IntLit(value: Int) extends Expr
    object IntLit extends ParserBridge1[Int, IntLit]

    case class BoolLit(value: Boolean) extends Expr
    object BoolLit extends ParserBridge1[Boolean, BoolLit]

    case class CharLit(value: Char) extends Expr
    object CharLit extends ParserBridge1[Char, CharLit]

    case class StrLit(value: String) extends Expr
    object StrLit extends ParserBridge1[String, StrLit]

    case class PairLit(value: String) extends Expr
    object PairLit extends ParserBridge1[String, PairLit]

    case class Ident(name: String) extends Expr with Lvalue
    object Ident extends ParserBridge1[String, Ident]

    case class ArrayElem(name: Ident, 
                         dimension: List[Expr]) extends Expr with Lvalue
    object ArrayElem extends ParserBridge2[Ident, List[Expr], ArrayElem]

  /* Separate things */
  sealed trait Lvalue
  sealed trait Rvalue

  case class NewPair(expr1: Expr, expr2: Expr) extends Rvalue
  object NewPair extends ParserBridge2[Expr, Expr, NewPair]

  case class Call(name: Ident, args: List[Expr]) extends Rvalue
  object Call extends ParserBridge2[Ident, List[Expr], Call]

  case class PairElem(lvalue: Lvalue) extends Lvalue with Rvalue
  object PairElem extends ParserBridge1[Lvalue, PairElem]
  
  case class ArrayLit(values: List[Expr]) extends Rvalue
  object ArrayLit extends ParserBridge1[List[Expr], ArrayLit]

  case class ArgList(values: List[Expr])
  object ArgList extends  ParserBridge1[List[Expr], ArgList]

  /* Statements */
  sealed trait Stat
    case class Skip() extends Stat

    case class Declare(type1: Type, name: Ident, rvalve: Rvalue) extends Stat
    object Declare extends ParserBridge3[Type, Ident, Rvalue, Declare]

    case class Assign(lvalue: Lvalue, rvalue: Rvalue) extends Stat
    object Assign extends ParserBridge2[Lvalue, Rvalue, Assign]

    case class Read(lvalue: Lvalue) extends Stat
    object Read extends ParserBridge1[Lvalue, Read]

    case class Free(expr: Expr) extends Stat
    object Free extends ParserBridge1[Expr, Free]

    case class Return(expr: Expr) extends Stat
    object Return extends ParserBridge1[Expr, Return]

    case class Exit(expr: Expr) extends Stat
    object Exit extends ParserBridge1[Expr, Exit]

    case class Print(expr: Expr) extends Stat
    object Print extends ParserBridge1[Expr, Print]

    case class Println(expr: Expr) extends Stat
    object Println extends ParserBridge1[Expr, Println]

    case class If(expr: Expr, stat1: List[Stat], stat2: List[Stat]) extends Stat
    object If extends ParserBridge3[Expr, List[Stat], List[Stat], If]

    case class While(expr: Expr, stat: List[Stat]) extends Stat
    object While extends ParserBridge2[Expr, List[Stat], While]

    case class Begin(stat: List[Stat]) extends Stat
    object Begin extends ParserBridge1[List[Stat], Begin]
 

  /* Function */
  case class Param(paramType: Type, ident: Ident)
  object Param extends ParserBridge2[Type, Ident, Param]

  case class Func(type1: Type, ident: Ident, params: List[Param], 
                  stats: List[Stat])
  object Func extends ParserBridge4[Type, Ident, List[Param], List[Stat], Func]

    




}