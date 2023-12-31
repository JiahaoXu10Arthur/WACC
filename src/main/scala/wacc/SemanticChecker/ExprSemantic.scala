package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer

import wacc.Ast._
import wacc.Error.SemanticErrorBuilder._
import wacc.Error.Errors._

import SemanticTypes._
import SymbolObject._
import SymbolObjectType._
import wacc.CodeGen.Utils.{isSelfAccess}

object ExprSemantic {

  def checkExpr(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    expr match {
      /* Arithmetic binary operators */
      case Mul(e1, e2) => arithmeticsCheck(e1, e2)
      case Div(e1, e2) => arithmeticsCheck(e1, e2)
      case Mod(e1, e2) => arithmeticsCheck(e1, e2)
      case Add(e1, e2) => arithmeticsCheck(e1, e2)
      case Sub(e1, e2) => arithmeticsCheck(e1, e2)

      /* Comparison binary operators */
      case Gt(e1, e2)  => compareCheck(e1, e2)
      case Gte(e1, e2) => compareCheck(e1, e2)
      case Lt(e1, e2)  => compareCheck(e1, e2)
      case Lte(e1, e2) => compareCheck(e1, e2)

      /* Equality binary operators */
      case Eq(e1, e2)  => eqCheck(e1, e2)
      case Neq(e1, e2) => eqCheck(e1, e2)

      /* Logical binary operators */
      case And(e1, e2) => logiCheck(e1, e2)
      case Or(e1, e2)  => logiCheck(e1, e2)

      /* Unary operators */
      case Not(e) => unaryCheck(e, BoolType(), BoolType())
      case Neg(e) => unaryCheck(e, IntType(), IntType())
      case Len(e) => lenCheck(e)
      case Ord(e) => unaryCheck(e, CharType(), IntType())
      case Chr(e) => unaryCheck(e, IntType(), CharType())

      /* Literals */
      case IntLit(_)  => IntType()
      case BoolLit(_) => BoolType()
      case CharLit(_) => CharType()
      case StrLit(_)  => StrType()
      case PairLit()  => PairType(AnyType(), AnyType())

      case expr: Ident               => identCheck(expr)
      case ArrayElem(ident, indexes) => arrayElemCheck(ident, indexes)
      case StructElem(ident, fields)  => structElemCheck(ident, fields)
    }
  }

  /* Argument1 type: Int,
     Argument2 type: Int,
     Return type   : Int */
  def arithmeticsCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {

    /* Check first argument is Int */
    val type1 = checkExpr(expr1)
    if (!equalType(type1, IntType())) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(IntType()),
        Seq("First sub-expression is not type int")
      )
    }

    /* Check second argument is Int */
    val type2 = checkExpr(expr2)
    if (!equalType(type2, IntType())) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(IntType()),
        Seq("Second sub-expression is not type int")
      )
    }

    IntType()
  }

  /* Argument1 type: Int/Char,
     Argument2 type: Same as Arg1,
     Return type   : Bool */
  def compareCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)

    /* Check arguments are same type */
    if (!equalType(type1, type2)) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(type1),
        Seq("Expressions are not the same type")
      )
    }

    /* Check first argument is Int or Char */
    if (!equalType(type1, IntType()) && !equalType(type1, CharType())) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(IntType(), CharType()),
        Seq("First sub-expression is not type int nor type char")
      )
    }

    /* Check second argument is Int or Char */
    if (!equalType(type2, IntType()) && !equalType(type2, CharType())) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(IntType(), CharType()),
        Seq("Second sub-expression is not type int nor type char")
      )
    }

    BoolType()
  }

  /* Argument1 type: T,
     Argument2 type: T,
     Return type   : Bool */
  def eqCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)

    /* Check arguments are the same type */
    if (!equalType(type1, type2)) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(type1),
        Seq("Expressions are not the same type ")
      )
    }
    BoolType()
  }

  /* Argument1 type: Bool,
     Argument2 type: Bool,
     Return type   : Bool */
  def logiCheck(expr1: Expr, expr2: Expr)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr1)
    val type2 = checkExpr(expr2)

    /* Check first argument is Bool */
    if (!equalType(type1, BoolType())) {
      semErr += buildTypeError(
        expr1.pos,
        type1,
        Set(BoolType()),
        Seq("First sub-expression is not type bool ")
      )
    }

    /* Check second argument is Bool */
    if (!equalType(type2, BoolType())) {
      semErr += buildTypeError(
        expr2.pos,
        type2,
        Set(BoolType()),
        Seq("Second sub-expression is not type bool ")
      )
    }

    BoolType()
  }

  /* Arg type: given
     Return type: given */
  def unaryCheck(expr: Expr, argType: Type, retType: Type)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val type1 = checkExpr(expr)

    /* Check the argument type matches given type */
    if (!equalType(type1, argType)) {
      semErr += buildTypeError(
        expr.pos,
        type1,
        Set(retType),
        Seq(s"Expression is not type $argType")
      )
    }

    retType
  }

  /* Arg type: T[]
     Return type: Int */
  def lenCheck(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    val type1 = checkExpr(expr)

    /* Check argument is an array */
    checkExpr(expr) match {
      case AnyType()    =>
      case ArrayType(_) =>
      case _ =>
        semErr += buildTypeError(
          expr.pos,
          type1,
          Set(ArrayType(AnyType())),
          Seq("Expression is not an array")
        )
    }
    IntType()
  }

  /* refer to st */
  def identCheck(
      ident: Ident
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    /* Search for identifier in all scope */
    st.lookUpAllIdent(ident.name) match {
      case Some(symObj) => symObj.getType()
      case None => {
            /* If cannot find, error */
            semErr += buildScopeError(
              ident.pos,
              ident.name,
              st.lookUpAllSimilar(ident.name, VariableType()),
              Seq(s"Variable ${ident.name} has not been declared in this scope")
            )
            /* Add a fake variable to avoid further error */
            st.add(
              ident.name,
              VariableType(),
              new VariableObj(AnyType(), ident.pos)
            )

            AnyType()
      }
    }

  }

  /* Arg1: Ident -> Refer to ArrayObj in st -> T[]
     Arg2: Int[] -> every element Int
     Return: T */
  def arrayElemCheck(ident: Ident, indexes: List[Expr])(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    val exprType = checkExpr(ident)

    /* Check every index is int */
    var returnType = exprType
    for (index <- indexes) {
      val indexType = checkExpr(index)
      indexType match {
        case IntType() =>
        case _ => {
          semErr += buildTypeError(
            index.pos,
            indexType,
            Set(IntType()),
            Seq("The index of an array needs to have type int")
          )
          returnType = AnyType()
        }
      }
    }

    var true_dimension = 0;
    /* Check correct dimension */
    for (index <- 0 until indexes.length) {
      returnType match {
        case AnyType() =>
        case ArrayType(elemType) => {
          true_dimension += 1
          returnType = elemType
        }
        case other => {
          /* If dimension mismatches , calculate the correct return type */
          val shouldType = createNestArrayType(other, indexes.length)
          semErr += buildTypeError(
            ident.pos,
            exprType,
            Set(shouldType),
            Seq(
              s"Incorrect array dimension \n" +
                s"Expected dimension: ${indexes.length} \n" +
                s"Actual dimension: $true_dimension"
            )
          )
          returnType = AnyType()
        }
      }
    }

    returnType
  }

  def createNestArrayType(innerType: Type, indexNum: Int): Type = {
    var returnType = innerType
    for (i <- 0 until indexNum) {
      returnType = ArrayType(returnType)
    }
    returnType
  }

  def structElemCheck(
    structName: Ident,
    fields: List[Ident])(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Type = {
    // If is self access, check as expression in class's symbol table
    if (isSelfAccess(structName)) {
      val class_st = st.findSecondLevelSt
      return checkExpr(fields.last)(class_st, semErr)
    }

    // Previous layer of symbol table
    var preSt = getStructTable(structName)

    val fieldNum = fields.length
    var index = 0

    // Check every field except the last is a struct
    while (index < fieldNum - 1) {
      // Find symbol table of the next struct
      preSt = extractFieldTable(fields(index), preSt)

      // If any field is not a struct, return AnyType
      preSt match {
        case None => return AnyType()
        case _ =>
      }
      index += 1
    }

    // If all fields are struct, type is the last field's type
    preSt match {
      case None => return AnyType()
      case Some(st) => checkExpr(fields.last)(preSt.get, semErr)
    }
  }

  /* Get the symbol table of the struct */
  private def extractFieldTable(fieldName: Ident, st: Option[SymbolTable])(
                                implicit semErr: ListBuffer[WACCError]): Option[SymbolTable] = {
     st match {
      case Some(struct_st) => getStructTable(fieldName)(struct_st, semErr)
      case None => None
     }
  }

  /* Get next layer of symbol table of struct */
  private def getStructTable(structName: Ident)(implicit st: SymbolTable, 
                                                         semErr: ListBuffer[WACCError]): Option[SymbolTable] = {
    // If not defined struct, return None
    var retTable: Option[SymbolTable] = None

		// Check the struct is struct type
    val structType = checkExpr(structName)
    structType match {
      case AnyType() => 
      case StructType(structName) => {
				// Check the struct is defined
				st.lookUpAll(structName.name, StructObjType()) match {
					case Some(obj: StructObj) => retTable = Some(obj.symTable)
					case _ => {
						semErr += buildScopeError(
							structName.pos,
							structName.name,
							st.lookUpAllSimilar(structName.name, StructObjType()),
							Seq(s"Struct ${structName.name} has not been declared in this scope")
						)
					} 
				}
			}
      case ClassType(className) => {
        // Check class is defined
        st.lookUpAll(className.name, ClassObjType()) match {
          // recursive call in class's symbol table
          case Some(obj: ClassObj) => retTable = getStructTable(className)(obj.symTable, semErr)
          case _ => {
            semErr += buildScopeError(
              className.pos,
              className.name,
              st.lookUpAllSimilar(className.name, ClassObjType()),
              Seq(s"Class ${className.name} has not been declared in this scope")
            )
          }
				}
      }
      case _ => {
        semErr += buildTypeError(
          structName.pos,
          structType,
          Set(StructType(structName)),
          Seq(s"$structName is not a struct but used as struct access")
        )
      }
    }

    retTable

  }
}
