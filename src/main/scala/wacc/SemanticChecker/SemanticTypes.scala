package wacc.SemanticChecker

import wacc.SyntaxChecker.Types
import wacc.Ast._

object SemanticTypes {
  sealed trait Type
  case class AnyType() extends Type {
    override def toString() = "Any"
  }

  case class IntType() extends Type {
    override def toString() = "Int"
  }
  case class BoolType() extends Type {
    override def toString() = "Bool"
  }
  case class CharType() extends Type {
    override def toString() = "Char"
  }
  case class StrType() extends Type {
    override def toString() = "String"
  }

  case class PairType(elem1: Type, elem2: Type) extends Type {
    override def toString() = s"Pair(${elem1.toString()}, ${elem2.toString()})"
  }

  case class ArrayType(elemType: Type) extends Type {
    override def toString() = s"${elemType.toString()}[]"
  }

  /* Type equality involve AnyType */
  def equalType(type1: Type, type2: Type): Boolean = {
    if (type1 == type2) {
      return true
    }

    var sameType = true
    (type1, type2) match {
      case (AnyType(), _) =>
      case (_, AnyType()) =>
      case (PairType(t1, t2), PairType(t3, t4)) => {
        sameType &&= equalType(t1, t3)
        sameType &&= equalType(t2, t4)
      }
      case (ArrayType(t1), ArrayType(t2)) => {
        sameType &&= equalType(t1, t2)
      }
      case _ => sameType = false
    }
    sameType
  }

  /* Convert syntax type to semantics type */
  def convertType(syntaxType: Types.Type): Type = {
    syntaxType match {
      case Types.IntType()        => IntType()
      case Types.BoolType()       => BoolType()
      case Types.CharType()       => CharType()
      case Types.StrType()        => StrType()
      case Types.PairType(t1, t2) => PairType(convertType(t1), convertType(t2))
      case Types.ArrayType(t)     => ArrayType(convertType(t))
    }
  }

  /* Convert special syntax type - pair elem type - to semantics type */
  def convertType(syntaxType: Types.PairElemType): Type = {
    syntaxType match {
      case Types.IntType()       => IntType()
      case Types.BoolType()      => BoolType()
      case Types.CharType()      => CharType()
      case Types.StrType()       => StrType()
      case Types.PairTypeIdent() => PairType(AnyType(), AnyType())
      case Types.ArrayType(t)    => ArrayType(convertType(t))
    }
  }


  def checkExprType(
      expr: Expr
    )(implicit st: SymbolTable): Type = {

    expr match {
      case Add(_, _)   => IntType()
      case Sub(_, _)   => IntType()
      case Mul(_, _)   => IntType()
      case Div(_, _)   => IntType()
      case Mod(_, _)   => IntType()

      case Gt(_, _)    => BoolType()
      case Gte(_, _)   => BoolType()
      case Lt(_, _)    => BoolType()
      case Lte(_, _)   => BoolType()
      case Eq(_, _)    => BoolType()
      case Neq(_, _)   => BoolType()

      case And(_, _)   => BoolType()
      case Or(_, _)    => BoolType()
      case Not(_)           => BoolType()
      case Neg(_)           => IntType()
      case Len(_)           => IntType()
      case Ord(_)           => IntType()
      case Chr(_)           => CharType()

      case IntLit(_)       => IntType()
      case BoolLit(_)      => BoolType()
      case CharLit(_)      => CharType()
      case StrLit(_)       => StrType()
      case PairLit()           => AnyType()
      case Ident(name)         => {
        /* Search for identifier in all scope */
        st.lookUpAllVar(name) match {
          case Some(symObj) => symObj.getType()
          case None => AnyType()
        }
      }
      case ArrayElem(ident, indexes) => {
        var returnType = checkExprType(ident)
        
        /* Check correct dimension */
        for (index <- 0 until indexes.length) {
          returnType match {
            case AnyType() =>
            case ArrayType(elemType) => {
              returnType = elemType
            }
            case other => AnyType()
          }
        } 
        returnType
      } 
    }
  }

  def checkLvalueType(
      lvalue: Lvalue
    )(implicit st: SymbolTable): Type = {
    lvalue match {
      case lvalue: Ident     => checkExprType(lvalue)
      case lvalue: ArrayElem => checkExprType(lvalue)
      case PairElem(index, lvalue) => {
        var returnType: Type = null
        val lType = checkLvalueType(lvalue)
        lType match {
          case PairType(t1, t2) =>
            index match {
              case "fst" => returnType = t1
              case "snd" => returnType = t2
          }
          case _ => AnyType()
        }

        returnType
      }
    }
  }

  def allArgsSameType(args1: List[Type],
                      args2: List[Type]): Boolean = {
    var sameType = true

    /* First check if number of arguments are the same */
    if (args1.size != args2.size) {
      sameType = false
    }

    var index = 0
    // Once different type occurs, overloading
    while (sameType && index < args1.size) {
      sameType = equalType(args1(index), args2(index))
      index += 1
    }

    sameType
  }
}
