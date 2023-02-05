package wacc
import Ast._
import SemanticType._
import SemanticChecker.semanticErr
import ValueSemantic._
import SymbolObject._

object StatSemantic {
  def checkState(stat: Stat, st: SymbolTable): Boolean = {
    stat match {
      /* check for declaration: what object is created? */
      case Declare(type1, ident, initValue) => declareCheck(type1, ident, initValue, st)
      case Assign(target, newValue) => assignCheck(target, newValue, st)
      case _ => false
    }
    true
  }

  /* Name must not clash keywords and other variables in scope 
     Initial value type should match type of variable*/
  def declareCheck(type1: Types.Type, ident: Ident, initValue: Rvalue, st: SymbolTable): Unit = {
    val semType: Type = convertType(type1)
    /* Check existence, Create new VariableObj */
    st.lookUp(ident.name) match {
      case Some(_) => semanticErr("Declare: Variable name already exists")
      case None => st.add(ident.name, VariableObj(semType))
    }

    /* Initial value should match the type */
    if (semType != checkRvalue(initValue, st)) {
      semanticErr("Declare: Initial value wrong type")
    }
  }

  /* Convert syntax type to semantics type */
  def convertType(syntaxType: Types.Type): Type = {
    syntaxType match {
      case Types.IntType()  => IntType()
      case Types.BoolType() => BoolType()
      case Types.CharType() => CharType()
      case Types.StrType()  => StrType()
      case Types.PairType(t1, t2) => PairType(convertPairElemType(t1), convertPairElemType(t2))
      case Types.ArrayType(t) => ArrayType(convertType(t))
    }
  }

  /* Convert special syntax type - pair elem type - to semantics type */
  def convertPairElemType(syntaxType: Types.PairElemType): Type = {
    syntaxType match {
      case Types.IntType()  => IntType()
      case Types.BoolType() => BoolType()
      case Types.CharType() => CharType()
      case Types.StrType()  => StrType()
      case Types.PairTypeIdent()  => PairType(AnyType(), AnyType())
      case Types.ArrayType(t) => ArrayType(convertType(t))
    }
  }

  def assignCheck(target: Lvalue, newValue: Rvalue, st: SymbolTable): Unit = {
    newValue match {
      case newValue: Expr => retCheck(target, newValue, st)
      case newValue: ArrayLit => retCheck(target, newValue, st)
      case newValue: Call => retCheck(target, newValue, st)
      case newValue: NewPair => retCheck(target, newValue, st)
      case newValue: PairElem => retCheck(target, newValue, st)
      case _ => semanticErr("Assign: wrong target type")
    }
  }

  /* Target type match newValue type */
  def retCheck(target: Lvalue, newValue: Rvalue, st: SymbolTable) = {
    if (checkLvalue(target, st) != checkRvalue(newValue, st)) {
      semanticErr("Assign: assign value mismatch target ")
    }
  }
  
  

}
