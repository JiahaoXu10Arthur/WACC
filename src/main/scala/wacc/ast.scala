package wacc

import ParserPositionBridge.{ParserSingletonBridgePos,ParserBridgePos1, ParserBridgePos2, ParserBridgePos3, ParserBridgePos4}
import Types._
import SymbolObject._

/* 现在的问题：
  1. Expr 有exprType，初始化为null，在某些case里会update（Literal）
  2. check（）进行semantic check，如果有error，直接报错返回，最后return 这个expr evaluate的type
     这一步会更改exprType，但需确认是否一定能够更改，并且access exprType得在更改后
     e.g. ！<expr> 里 expr可以是个ident，ident必须call过check（）之后，才能更新type，不然永远是null
  3. 可否把check（）变成Expr的默认function，需要考虑，有的check（）take in symbolTable，有的不需要
  4. 怎么define symbol table？是永远check（）take in parameter，还是有一个global？怎么test？
*/

object Ast {
  /* Global symbol table */
  val st = new SymbolTable(null)

  /* Program */  
  case class Program(funcs: List[Func], stats: List[Stat])(val pos: (Int, Int))
  object Program extends ParserBridgePos2[List[Func], List[Stat], Program]

  /* Binary Expressions */
  sealed trait Expr extends Rvalue {
    // var exprType: Type = null
    def check(st: SymbolTable): Type = {
      null
    }

  }
    /* Arithmetic binary operators */
    case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Mul extends ParserBridgePos2[Expr, Expr, Mul]

    case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Div extends ParserBridgePos2[Expr, Expr, Div]

    case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Mod extends ParserBridgePos2[Expr, Expr, Mod]
    
    case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Add extends ParserBridgePos2[Expr, Expr, Add]
    
    case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Sub extends ParserBridgePos2[Expr, Expr, Sub]
    
    // /* Comparison binary operators */
    case class Gt(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Gt extends ParserBridgePos2[Expr, Expr, Gt]
    
    case class Gte(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Gte extends ParserBridgePos2[Expr, Expr, Gte]
    
    case class Lt(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Lt extends ParserBridgePos2[Expr, Expr, Lt]
    
    case class Lte(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Lte extends ParserBridgePos2[Expr, Expr, Lte]

    case class Eq(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Eq extends ParserBridgePos2[Expr, Expr, Expr]

    case class Neq(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Neq extends ParserBridgePos2[Expr, Expr, Neq]

    /* Logical binary operators */
    case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object And extends ParserBridgePos2[Expr, Expr, And]
    
    case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr
    object Or extends ParserBridgePos2[Expr, Expr, Or]
    
  /* Unary operators */
  sealed trait Term extends Expr
    case class Not(expr: Expr)(val pos: (Int, Int)) extends Expr {
      /* Arugument type: bool
         Return type: bool */
      override def check(st: SymbolTable): Type = {
        if (expr.check(st) != BoolType()) {
          semanticErr("Not argument not bool")
        }
        BoolType()
      }
    }
    object Not extends ParserBridgePos1[Expr, Not]

    case class Neg(expr: Expr)(val pos: (Int, Int)) extends Expr {
      /* Arugument type: int
         Return type: int */
      override def check(st: SymbolTable): Type = {
        if (expr.check(st) != IntType()) {
          semanticErr("Neg argument not int")
        }
        IntType()
      }
    }
    object Neg extends ParserBridgePos1[Expr, Neg]

    case class Len(expr: Expr)(val pos: (Int, Int)) extends Expr {
      /* Arugument type: T[]
         Return type: int */
      override def check(st: SymbolTable): Type = {
        expr.check(st) match {
          case ArrayType(_) => 
          case _ => semanticErr("Len argument not array")
        }
        IntType()
      }
    }
    object Len extends ParserBridgePos1[Expr, Len]

    case class Ord(expr: Expr)(val pos: (Int, Int)) extends Expr {
      /* Arugument type: char
         Return type: int */
      override def check(st: SymbolTable): Type = {
        if (expr.check(st) != CharType()) {
          semanticErr("Ord argument not char")
        }
        IntType()
      }
    }
    object Ord extends ParserBridgePos1[Expr, Ord]

    case class Chr(expr: Expr)(val pos: (Int, Int)) extends Expr {
      /* Arugument type: int
         Return type: char */
      override def check(st: SymbolTable): Type = {
        if (expr.check(st) != IntType()) {
          semanticErr("Chr argument not int")
        }
        CharType()
      }
    }
    object Chr extends ParserBridgePos1[Expr, Chr]

  /* Literals */
  sealed trait Atom extends Term    
    case class IntLit(value: Int)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = IntType()
    }
    object IntLit extends ParserBridgePos1[Int, IntLit] 

    case class BoolLit(value: Boolean)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = BoolType()
    }
    object BoolLit extends ParserBridgePos1[Boolean, BoolLit]

    case class CharLit(value: Char)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = CharType()
    }
    object CharLit extends ParserBridgePos1[Char, CharLit]

    case class StrLit(value: String)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = StrType()
    }
    object StrLit extends ParserBridgePos1[String, StrLit]

    case class PairLit()(val pos: (Int, Int)) extends Expr {
      /* Pair Elem type extends Type? */
      // def check(): Type = PairTypeIdent()
    }
    object PairLit extends ParserSingletonBridgePos[PairLit] {
      override def con(pos: (Int, Int)) = this()(pos)
    }

    case class Ident(name: String)(val pos: (Int, Int)) extends Expr with Lvalue {
      // exprType = check(st)
      override def check(st: SymbolTable): Type = {
        st.lookUp(name) match {
          case Some(symObj) => symObj.getType()
          case None => semanticErr("Ident not in symbol table")
        }
      }
    }
    object Ident extends ParserBridgePos1[String, Ident]

    case class ArrayElem(ident: Ident, 
                         index: List[Expr])
                         (val pos: (Int, Int)) extends Expr with Lvalue {
      override def check(st: SymbolTable): Type = {
        var returnType: Type = null

        /* First argument type should be T[] */
        st.lookUp(ident.name) match {
          case Some(symObj) => {
            /* Return type should be T */
            if (!symObj.isInstanceOf[ArrayObj]) {
              semanticErr("ArrayElem check arrayObj")
            } else {
              returnType = symObj.getType()
            }
          }
          case None => semanticErr("ArrayElem not in symbol table")
        }

        /* Second argument should be Int */
        index.foreach{x => if (x.check(st) != IntType()) 
                               { semanticErr("ArrayElem index not int type")}}

        /* If out of bound, run time error */
        returnType
      }
    }
    object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]

  /* Separate things */
  sealed trait Lvalue
  sealed trait Rvalue

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue
  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]

  case class Call(name: Ident, args: List[Expr])(val pos: (Int, Int)) extends Rvalue
  object Call extends ParserBridgePos2[Ident, List[Expr], Call]

  case class PairElem(lvalue: Lvalue)(val pos: (Int, Int)) extends Lvalue with Rvalue
  object PairElem extends ParserBridgePos1[Lvalue, PairElem]
  
  case class ArrayLit(values: List[Expr])(val pos: (Int, Int)) extends Rvalue
  object ArrayLit extends ParserBridgePos1[List[Expr], ArrayLit]

  case class ArgList(values: List[Expr])(val pos: (Int, Int))
  object ArgList extends  ParserBridgePos1[List[Expr], ArgList]

  /* Statements */
  sealed trait Stat
    case class Skip()(val pos: (Int, Int)) extends Stat
    object Skip extends ParserSingletonBridgePos[Skip] {
      override def con(pos: (Int, Int)) = this()(pos)
    }

    case class Declare(type1: Type, name: Ident, rvalve: Rvalue)(val pos: (Int, Int)) extends Stat
    object Declare extends ParserBridgePos3[Type, Ident, Rvalue, Declare]

    case class Assign(lvalue: Lvalue, rvalue: Rvalue)(val pos: (Int, Int)) extends Stat
    object Assign extends ParserBridgePos2[Lvalue, Rvalue, Assign]

    case class Read(lvalue: Lvalue)(val pos: (Int, Int)) extends Stat
    object Read extends ParserBridgePos1[Lvalue, Read]

    case class Free(expr: Expr)(val pos: (Int, Int)) extends Stat
    object Free extends ParserBridgePos1[Expr, Free]

    case class Return(expr: Expr)(val pos: (Int, Int)) extends Stat
    object Return extends ParserBridgePos1[Expr, Return]

    case class Exit(expr: Expr)(val pos: (Int, Int)) extends Stat
    object Exit extends ParserBridgePos1[Expr, Exit]

    case class Print(expr: Expr)(val pos: (Int, Int)) extends Stat
    object Print extends ParserBridgePos1[Expr, Print]

    case class Println(expr: Expr)(val pos: (Int, Int)) extends Stat
    object Println extends ParserBridgePos1[Expr, Println]

    case class If(expr: Expr, stat1: List[Stat], stat2: List[Stat])
                 (val pos: (Int, Int)) extends Stat
    object If extends ParserBridgePos3[Expr, List[Stat], List[Stat], If]

    case class While(expr: Expr, stat: List[Stat])(val pos: (Int, Int)) extends Stat
    object While extends ParserBridgePos2[Expr, List[Stat], While]

    case class Begin(stat: List[Stat])(val pos: (Int, Int)) extends Stat
    object Begin extends ParserBridgePos1[List[Stat], Begin]
 

  /* Function */
  case class Param(paramType: Type, ident: Ident)(val pos: (Int, Int))
  object Param extends ParserBridgePos2[Type, Ident, Param]

  case class Func(type1: Type, ident: Ident, params: List[Param], 
                  stats: List[Stat])(val pos: (Int, Int))
  object Func extends ParserBridgePos4[Type, Ident, List[Param], List[Stat], Func]

  def semanticErr(where: String) = {
    throw new SemanticErr("Semantic Error in " + where)
  }

  case class SemanticErr(private val message: String = "Semantic Error", 
                         private val cause: Throwable = None.orNull)
                         extends Exception(message, cause) 

}