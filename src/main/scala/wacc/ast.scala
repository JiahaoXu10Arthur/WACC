package wacc

import parsley.genericbridges.{ParserSingletonBridge, ParserBridge1, 
                               ParserBridge2, ParserBridge3}

object Ast {

  /* Binary Expressions */
  sealed trait Expr
    /* Arithmetic binary operators */
    case class Mul(expr1: Expr, expr2: Expr) extends Expr
    object Mul extends ParserBridge2[Expr, Expr, Expr]

    case class Div(expr1: Expr, expr2: Expr) extends Expr
    object Div extends ParserBridge2[Expr, Expr, Expr]

    case class Mod(expr1: Expr, expr2: Expr) extends Expr
    object Mod extends ParserBridge2[Expr, Expr, Expr]
    
    case class Add(expr1: Expr, expr2: Expr) extends Expr
    object Add extends ParserBridge2[Expr, Expr, Expr]
    
    case class Sub(expr1: Expr, expr2: Expr) extends Expr
    object Sub extends ParserBridge2[Expr, Expr, Expr]
    
    /* Comparison binary operators */
    case class Gt(expr1: Expr, expr2: Expr) extends Expr
    object Gt extends ParserBridge2[Expr, Expr, Expr]
    
    case class Gte(expr1: Expr, expr2: Expr) extends Expr
    object Gte extends ParserBridge2[Expr, Expr, Expr]
    
    case class Lt(expr1: Expr, expr2: Expr) extends Expr
    object Lt extends ParserBridge2[Expr, Expr, Expr]
    
    case class Lte(expr1: Expr, expr2: Expr) extends Expr
    object Lte extends ParserBridge2[Expr, Expr, Expr]

    case class Eq(expr1: Expr, expr2: Expr) extends Expr
    object Eq extends ParserBridge2[Expr, Expr, Expr]

    case class Neq(expr1: Expr, expr2: Expr) extends Expr
    object Neq extends ParserBridge2[Expr, Expr, Expr]

    /* Logical binary operators */
    case class And(expr1: Expr, expr2: Expr) extends Expr
    object And extends ParserBridge2[Expr, Expr, Expr]
    
    case class Or(expr1: Expr, expr2: Expr) extends Expr
    object Or extends ParserBridge2[Expr, Expr, Expr]

  /* Unary operators */
  sealed trait Term extends Expr
    case class Not(expr: Expr) extends Expr
    object Not extends ParserBridge1[Expr, Expr]

    case class Neg(expr: Expr) extends Expr
    object Neg extends ParserBridge1[Expr, Expr]

    case class Len(expr: Expr) extends Expr
    object Len extends ParserBridge1[Expr, Expr]

    case class Ord(expr: Expr) extends Expr
    object Ord extends ParserBridge1[Expr, Expr]

    case class Chr(expr: Expr) extends Expr
    object Chr extends ParserBridge1[Expr, Expr]

  /* Literals */
  sealed trait Atom extends Term    
    case class IntLit(value: Int) extends Expr
    object IntLit extends ParserBridge1[Int, Expr]

    case class BoolLit(value: Boolean) extends Expr
    object BoolLit extends ParserBridge1[Boolean, Expr]

    case class CharLit(value: Char) extends Expr
    object CharLit extends ParserBridge1[Char, Expr]

    case class StrLit(value: String) extends Expr
    object StrLit extends ParserBridge1[String, Expr]

    case class PairLit(value: String) extends Expr
    object PairLit extends ParserBridge1[String, Expr]

    case class Ident(name: String) extends Expr
    object Ident extends ParserBridge1[String, Expr]

    case class ArrayElem(array: Expr, index: Expr) extends Expr
    object ArrayElem extends ParserBridge2[Expr, Expr, Expr]

  /* Statements */
  sealed trait Stat
    case class Skip() extends Stat

    case class Declare(type1: Type, name: Ident, rvalve: Type) extends Stat
    object Declare extends ParserBridge3[Type, Ident, Type, Stat]

    case class Assign(lvalue: Type, rvalue: Type) extends Stat
    object Assign extends ParserBridge2[Type, Type, Stat]

    case class Read(lvalue: Type) extends Stat
    object Read extends ParserBridge1[Type, Stat]

    case class Free(expr: Expr) extends Stat
    object Free extends ParserBridge1[Expr, Stat]

    case class Return(expr: Expr) extends Stat
    object Return extends ParserBridge1[Expr, Stat]

    case class Exit(expr: Expr) extends Stat
    object Exit extends ParserBridge1[Expr, Stat]

    case class Print(expr: Expr) extends Stat
    object Print extends ParserBridge1[Expr, Stat]

    case class Println(expr: Expr) extends Stat
    object Println extends ParserBridge1[Expr, Stat]

    case class If(expr: Expr, stat1: List[Stat], stat2: List[Stat]) extends Stat
    object If extends ParserBridge3[Expr, List[Stat], List[Stat], Stat]

    case class While(expr: Expr, stat: List[Stat]) extends Stat
    object While extends ParserBridge2[Expr, List[Stat], Stat]

    case class Begin(stat: List[Stat]) extends Stat
    object Begin extends ParserBridge1[List[Stat], Stat]

  /* Types */
  sealed trait Type
    case class IntType() extends Type
    case class BoolType() extends Type
    case class CharType() extends Type
    case class StrType() extends Type
    case class ArrayType(arrayType: Type) extends Type
    case class PairType(value1Type: Type, value2Type: Type) extends Type
    

}