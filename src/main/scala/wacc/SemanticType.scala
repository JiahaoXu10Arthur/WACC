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
}
