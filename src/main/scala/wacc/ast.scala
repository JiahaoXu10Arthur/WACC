package wacc

import wacc.SyntaxChecker.ParserPositionBridge.{
  ParserBridgePos1,
  ParserBridgePos2,
  ParserBridgePos3,
  ParserBridgePos4,
  ParserSingletonBridgePos
}
import wacc.SyntaxChecker.Types._
import wacc.SemanticChecker.SymbolTable
import wacc.SemanticChecker.SemanticTypes

object Ast {
  /* Program */
  case class Program(structs: List[Struct], classes: List[Class], funcs: List[Func], stats: List[Stat])(val pos: (Int, Int))
  object Program extends ParserBridgePos4[List[Struct], List[Class], List[Func], List[Stat], Program]

  /* Binary Expressions */
  sealed trait Expr extends Rvalue {
    def pos: (Int, Int)
  }
  /* Arithmetic binary operators */
  case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Mul                                                    extends ParserBridgePos2[Expr, Expr, Mul]

  case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Div                                                    extends ParserBridgePos2[Expr, Expr, Div]

  case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Mod                                                    extends ParserBridgePos2[Expr, Expr, Mod]

  case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Add                                                    extends ParserBridgePos2[Expr, Expr, Add]

  case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Sub                                                    extends ParserBridgePos2[Expr, Expr, Sub]

  /* Comparison binary operators */
  case class Gt(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Gt                                                    extends ParserBridgePos2[Expr, Expr, Gt]

  case class Gte(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Gte                                                    extends ParserBridgePos2[Expr, Expr, Gte]

  case class Lt(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Lt                                                    extends ParserBridgePos2[Expr, Expr, Lt]

  case class Lte(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Lte                                                    extends ParserBridgePos2[Expr, Expr, Lte]

  case class Eq(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Eq                                                    extends ParserBridgePos2[Expr, Expr, Expr]

  case class Neq(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Neq                                                    extends ParserBridgePos2[Expr, Expr, Neq]

  /* Logical binary operators */

  case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object And                                                    extends ParserBridgePos2[Expr, Expr, And]

  case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
  object Or                                                    extends ParserBridgePos2[Expr, Expr, Or]

  /* Unary operators */
  sealed trait Term                               extends Expr
  case class Not(expr: Expr)(val pos: (Int, Int)) extends Expr
  object Not                                      extends ParserBridgePos1[Expr, Not]

  case class Neg(expr: Expr)(val pos: (Int, Int)) extends Expr
  object Neg                                      extends ParserBridgePos1[Expr, Neg]

  case class Len(expr: Expr)(val pos: (Int, Int)) extends Expr
  object Len                                      extends ParserBridgePos1[Expr, Len]

  case class Ord(expr: Expr)(val pos: (Int, Int)) extends Expr
  object Ord                                      extends ParserBridgePos1[Expr, Ord]

  case class Chr(expr: Expr)(val pos: (Int, Int)) extends Expr
  object Chr                                      extends ParserBridgePos1[Expr, Chr]

  /* Literals */
  sealed trait Atom                                  extends Term
  case class IntLit(value: Int)(val pos: (Int, Int)) extends Expr
  object IntLit                                      extends ParserBridgePos1[Int, IntLit]

  case class BoolLit(value: Boolean)(val pos: (Int, Int)) extends Expr
  object BoolLit                                          extends ParserBridgePos1[Boolean, BoolLit]

  case class CharLit(value: Char)(val pos: (Int, Int)) extends Expr
  object CharLit                                       extends ParserBridgePos1[Char, CharLit]

  case class StrLit(value: String)(val pos: (Int, Int)) extends Expr
  object StrLit                                         extends ParserBridgePos1[String, StrLit]

  case class PairLit()(val pos: (Int, Int)) extends Expr
  object PairLit extends ParserSingletonBridgePos[PairLit] {
    override def con(pos: (Int, Int)) = this()(pos)
  }
  case class Ident(name: String)(val pos: (Int, Int)) extends Expr with Lvalue
  object Ident extends ParserBridgePos1[String, Ident]

  case class ArrayElem(ident: Ident, index: List[Expr])(val pos: (Int, Int)) extends Expr with Lvalue
  object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]


  /* call a struct by "." */
  case class StructElem(ident: Ident, field: List[Ident])(val pos: (Int, Int))
      extends Expr
      with Lvalue
  object StructElem extends ParserBridgePos2[Ident, List[Ident], StructElem]

  /* Values */
  sealed trait Lvalue {
    def pos: (Int, Int)
  }
  sealed trait Rvalue {
    def pos: (Int, Int)
  }

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue
  object NewPair                                                    extends ParserBridgePos2[Expr, Expr, NewPair]

  case class Call(ident: Ident, args: List[Expr])(val pos: (Int, Int))
      extends Rvalue {
    /* Will be changed after a call's return type is known
       Added after function overloading
       The type is initialised to anytype. */
    var returnType: SemanticTypes.Type = SemanticTypes.AnyType()
  }
  object Call extends ParserBridgePos2[Ident, List[Expr], Call]

  case class PairElem(index: String, lvalue: Lvalue)(val pos: (Int, Int)) extends Lvalue with Rvalue
  object PairElem extends ParserBridgePos2[String, Lvalue, PairElem]

  case class ArrayLit(values: List[Expr])(val pos: (Int, Int)) extends Rvalue
  object ArrayLit extends ParserBridgePos1[List[Expr], ArrayLit]

  case class StructLit(values: List[Expr])(val pos: (Int, Int)) extends Rvalue
  object StructLit extends ParserBridgePos1[List[Expr], StructLit]

  case class ArgList(values: List[Expr])(val pos: (Int, Int))
  object ArgList extends ParserBridgePos1[List[Expr], ArgList]

  /* Statements */
  sealed trait Stat {
    var symb: SymbolTable = null
  }
  case class Skip()(val pos: (Int, Int)) extends Stat
  object Skip extends ParserSingletonBridgePos[Skip] {
    override def con(pos: (Int, Int)) = this()(pos)
  }

  case class Declare(type1: Type, ident: Ident, initValue: Rvalue)(
      val pos: (Int, Int)
  ) extends Stat
  object Declare extends ParserBridgePos3[Type, Ident, Rvalue, Declare]

  case class Assign(target: Lvalue, newValue: Rvalue)(val pos: (Int, Int)) extends Stat
  object Assign extends ParserBridgePos2[Lvalue, Rvalue, Assign]

  case class Read(lvalue: Lvalue)(val pos: (Int, Int)) extends Stat
  object Read                                          extends ParserBridgePos1[Lvalue, Read]

  case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat
  object Free                                      extends ParserBridgePos1[Expr, Free]

  case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat
  object Return                                      extends ParserBridgePos1[Expr, Return]

  case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat
  object Exit                                      extends ParserBridgePos1[Expr, Exit]

  case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat
  object Print                                      extends ParserBridgePos1[Expr, Print]

  case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat
  object Println                                      extends ParserBridgePos1[Expr, Println]

  case class If(expr: Expr, stat1: List[Stat], stat2: List[Stat])(
      val pos: (Int, Int)
  ) extends Stat
  object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]

  case class While(expr: Expr, stat: List[Stat])(val pos: (Int, Int)) extends Stat
  object While                                                        extends ParserBridgePos2[Expr, List[Stat], While]

  case class Begin(stat: List[Stat])(val pos: (Int, Int)) extends Stat
  object Begin                                            extends ParserBridgePos1[List[Stat], Begin]

  /* Function */
  case class Param(paramType: Type, ident: Ident)(val pos: (Int, Int))
  object Param extends ParserBridgePos2[Type, Ident, Param]

  case class Func(
      type1: Type,
      ident: Ident,
      params: List[Param],
      stats: List[Stat]
  )(val pos: (Int, Int)) {
    var symb: SymbolTable = null
  }
  object Func extends ParserBridgePos4[Type, Ident, List[Param], List[Stat], Func]
  
  /* Auxillary AST nodes for optimisation -- not parsed */
  case class TailRecurse(call: Call) extends Stat

  case class Struct(name: Ident, fields: List[(Type, Ident)])(val pos: (Int, Int)) {
    var symb: SymbolTable = null
  }
  object Struct extends ParserBridgePos2[Ident, List[(Type, Ident)], Struct]

  case class Class(
    struct: Struct,
    funcs: List[Func]
  )(val pos: (Int, Int)) {
    var symb: SymbolTable = null
  }
  object Class extends ParserBridgePos2[Struct, List[Func], Class]

}
