package wacc

import ParserPositionBridge.{ParserSingletonBridgePos,ParserBridgePos1, ParserBridgePos2, ParserBridgePos3, ParserBridgePos4}
import Types._
import SymbolObject._

object Ast {
  /* Program */  
  case class Program(funcs: List[Func], stats: List[Stat])(val pos: (Int, Int))
  object Program extends ParserBridgePos2[List[Func], List[Stat], Program]

  /* Binary Expressions */
  sealed trait Expr extends Rvalue {
    def check(st: SymbolTable): Type
  }
    /* Arithmetic binary operators */
    case class Mul(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = 
        arithmeticsCheck(expr1, expr2, st)
    }
    object Mul extends ParserBridgePos2[Expr, Expr, Mul]

    case class Div(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = 
        arithmeticsCheck(expr1, expr2, st)
    }
    object Div extends ParserBridgePos2[Expr, Expr, Div]

    case class Mod(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = 
        arithmeticsCheck(expr1, expr2, st)
    }
    object Mod extends ParserBridgePos2[Expr, Expr, Mod]
    
    case class Add(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = 
        arithmeticsCheck(expr1, expr2, st)
    }
    object Add extends ParserBridgePos2[Expr, Expr, Add]
    
    case class Sub(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = 
        arithmeticsCheck(expr1, expr2, st)
    }
    object Sub extends ParserBridgePos2[Expr, Expr, Sub]
    
    // /* Comparison binary operators */
    case class Gt(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        compareCheck(expr1, expr2, st)
      }
    }
    object Gt extends ParserBridgePos2[Expr, Expr, Gt]
    
    case class Gte(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        compareCheck(expr1, expr2, st)
      }
    }
    object Gte extends ParserBridgePos2[Expr, Expr, Gte]
    
    case class Lt(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        compareCheck(expr1, expr2, st)
      }
    }
    object Lt extends ParserBridgePos2[Expr, Expr, Lt]
    
    case class Lte(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        compareCheck(expr1, expr2, st)
      }
    }
    object Lte extends ParserBridgePos2[Expr, Expr, Lte]

    case class Eq(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        eqCheck(expr1, expr2, st)
      }
    }
    object Eq extends ParserBridgePos2[Expr, Expr, Expr]

    case class Neq(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        eqCheck(expr1, expr2, st)
      }
    }
    object Neq extends ParserBridgePos2[Expr, Expr, Neq]

    /* Logical binary operators */
    case class And(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        logiCheck(expr1, expr2, st)
      }
    }
    object And extends ParserBridgePos2[Expr, Expr, And]
    
    case class Or(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Expr {
      override def check(st: SymbolTable): Type = {
        logiCheck(expr1, expr2, st)
      }
    }
    object Or extends ParserBridgePos2[Expr, Expr, Or]
    
  /* Unary operators */
  sealed trait Term extends Expr
    case class Not(expr: Expr)(val pos: (Int, Int)) extends Expr {
      /* Arugument type: bool
         Return type: bool */
      override def check(st: SymbolTable): Type = {
        if (expr.check(st) != BoolType()) {
          semanticErr("Not: argument not bool")
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
          semanticErr("Neg: argument not int")
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
          case _ => semanticErr("Len: argument not array")
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
          semanticErr("Ord: argument not char")
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
          semanticErr("Chr: argument not int")
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
      override def check(st: SymbolTable): Type = PairType(AnyType(), AnyType())
    }
    object PairLit extends ParserSingletonBridgePos[PairLit] {
      override def con(pos: (Int, Int)) = this()(pos)
    }

    case class Ident(name: String)(val pos: (Int, Int)) extends Expr with Lvalue {
      override def check(st: SymbolTable): Type = {
        st.lookUp(name) match {
          case Some(symObj) => symObj.getType()
          case None => semanticErr("Ident: not in symbol table")
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
            symObj match {
            /* Return type should be T */
            case symObj: ArrayObj => returnType = symObj.elemType
            case _ => semanticErr("ArrayElem: fst arg not arrayObj")
          }
        }

          case None => semanticErr("ArrayElem: not in symbol table")
        }

        /* Second argument should be Int */
        index.foreach{x => if (x.check(st) != IntType()) 
                               { semanticErr("ArrayElem: index not int type")}}

        /* If out of bound, run time error */
        returnType
      }
    }
    object ArrayElem extends ParserBridgePos2[Ident, List[Expr], ArrayElem]

  /* Separate things */
  sealed trait Lvalue {
    def check(st: SymbolTable): Type = {null}
  }
  sealed trait Rvalue {
    def check(st: SymbolTable): Type = {null}
  }

  case class NewPair(expr1: Expr, expr2: Expr)(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable): Type = {
      val type1 = castToPairElem(expr1.check(st))
      val type2 = castToPairElem(expr2.check(st))

      PairType(type1, type2)
    }

    def castToPairElem(t: Type): PairElemType = {
      t match {
        case t: PairElemType => t
        case _ => semanticErr("New Pair: arg not PairElemType")
      }
    }
  }
  object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]

  case class Call(ident: Ident, args: List[Expr])(val pos: (Int, Int)) extends Rvalue {
    override def check(st: SymbolTable): Type = {
      var funcObj: FuncObj = null
      /* Find funcObj */
      st.lookUpAll(ident.name) match {
        case Some(symObj) =>  { 
            symObj match {
            case symObj: FuncObj => funcObj = symObj
            case _ => semanticErr("ArrayElem: fst arg not funcObj")
          }
        }
        case None => semanticErr("Call: function name not in symbol table")
      }

      /* check number of parameters */
      if (args.length != funcObj.argc) {
        semanticErr("Call: wrong argument number")
      }

      /* check every parameter's type */
      for (i <- 0 until args.length) {
        checkValueRef(args(i), st)
        if (args(i).check(st) != funcObj.args(i).getType()) {
          semanticErr("Call: function name not in symbol table")
        }
      }

      /* Return type */
      funcObj.returnType
    }
  
    /* Check pass parameter by value or by ref */
    def checkValueRef(expr: Expr, st: SymbolTable) = {
      expr match {
        // Int, Bool, Char should by value
        case IntLit(_) | BoolLit(_) | CharLit(_)  =>
        // String, Array, Pair should by reference
        case Ident(_) => expr.check(st) match {
          case StrType() =>
          case ArrayType(_) =>
          case PairType(_, _) =>
          case _ => semanticErr("Call: wrong by value Or by reference")
        }
        case _ => semanticErr("Call: wrong by value Or by reference")
      }
    }
  }
  object Call extends ParserBridgePos2[Ident, List[Expr], Call]

  case class PairElem(index: String, lvalue: Lvalue)(val pos: (Int, Int)) extends Lvalue with Rvalue {
    override def check(st: SymbolTable): Type = {
      var returnType: Type = null

      /* Lvalue should be a pair */
      val lType = lvalue.check(st)
      lType match {
        case PairType(t1, t2) => index match {
          case "fst" => returnType = castToType(t1)
          case "snd" => returnType = castToType(t2)
        }
        case _ => semanticErr("Pair elem: not Pair Type")
      }

      returnType
    }

    def castToType(t: PairElemType): Type = {
      t match {
        case t: Type => t
        case _ => semanticErr("Pair elem: arg not Type")
      }
    }
  }
  object PairElem extends ParserBridgePos2[String, Lvalue, PairElem]
  
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

  /* Argument1 type: Int,
     Argument2 type: Int,
     Return type   : Int */
  def arithmeticsCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (expr1.check(st) == IntType()) {
        if (expr2.check(st) == IntType()) {
          /* return type */
          IntType()
        } else {
          semanticErr("ArithBio: Expr2 not int type")
        }
      } else {
        semanticErr("ArithBio: Expr1 not int type")
    }
  }

  /* Argument1 type: Int/Char,
     Argument2 type: Same as Arg1,
     Return type   : Bool */
  def compareCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (expr1.check(st) == expr2.check(st)) {
      if (expr1.check(st) == IntType() || expr1.check(st) == CharType()) {
        /* return type */
        BoolType()
      } else {
        semanticErr("CompBio: Both expressions are not int or char type")
      }
    } else {
      semanticErr("CompBio: Both expressions are not of the same type")
    }
  }

  /* Argument1 type: T,
     Argument2 type: T,
     Return type   : Bool */
  def eqCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (expr1.check(st) == expr2.check(st)) {
      BoolType()
    } else {
      semanticErr("EqBio: Both expressions are not of the same type")
    }
  }

  /* Argument1 type: Bool,
     Argument2 type: Bool,
     Return type   : Bool */
  def logiCheck(expr1: Expr, expr2: Expr, st: SymbolTable): Type = {
    if (expr1.check(st) == BoolType()) {
        if (expr2.check(st) == BoolType()) {
          /* return type */
          BoolType()
        } else {
          semanticErr("LogiBio: Expr2 not bool type")
        }
      } else {
        semanticErr("LogiBio: Expr1 not bool type")
    }
  }
}