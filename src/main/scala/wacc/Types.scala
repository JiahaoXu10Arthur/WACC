package wacc

object Types {
  sealed trait Type
    case class PairType(elem1: PairElemType, elem2: PairElemType) extends Type
    case class ArrayType(elemType: Type) extends Type with PairElemType
     
  sealed trait PairElemType
    case class PairTypeIdent() extends PairElemType

  sealed trait BasicType extends Type with PairElemType
    case class IntType() extends BasicType
    case class BoolType() extends BasicType
    case class CharType() extends BasicType
    case class StrType() extends BasicType

  case class AnyType() extends Type with PairElemType
}
