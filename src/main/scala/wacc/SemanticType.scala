package wacc

object SemanticType {
  sealed trait Type
  case class AnyType() extends Type

  case class IntType() extends Type
  case class BoolType() extends Type
  case class CharType() extends Type
  case class StrType() extends Type

  case class PairType(elem1: Type, elem2: Type) extends Type

  case class ArrayType(elemType: Type) extends Type

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

}
