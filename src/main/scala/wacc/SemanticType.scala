package wacc

object SemanticType {
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

    if (type1 == AnyType() || type2 == AnyType()) {
      return true
    }

    var sameType = true
    (type1, type2) match {
      case (PairType(t1, t2), PairType(t3, t4)) => {
        sameType &&= equalType(t1, t3)
        sameType &&= equalType(t2, t4)
      }
      case (ArrayType(t1), ArrayType(t2)) => {
        sameType &&= equalType(t1, t2)
      }
      case _ => sameType = false
    }
    return sameType
  }

    /* Convert syntax type to semantics type */
  def convertType(syntaxType: Types.Type): Type = {
    syntaxType match {
      case Types.IntType()  => IntType()
      case Types.BoolType() => BoolType()
      case Types.CharType() => CharType()
      case Types.StrType()  => StrType()
      case Types.PairType(t1, t2) => PairType(convertType(t1), convertType(t2))
      case Types.ArrayType(t) => ArrayType(convertType(t))
    }
  }

  /* Convert special syntax type - pair elem type - to semantics type */
  def convertType(syntaxType: Types.PairElemType): Type = {
    syntaxType match {
      case Types.IntType()  => IntType()
      case Types.BoolType() => BoolType()
      case Types.CharType() => CharType()
      case Types.StrType()  => StrType()
      case Types.PairTypeIdent()  => PairType(AnyType(), AnyType())
      case Types.ArrayType(t) => ArrayType(convertType(t))
    }
  }

}
