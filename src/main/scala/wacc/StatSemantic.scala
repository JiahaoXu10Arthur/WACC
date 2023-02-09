package wacc
import Ast._
import SemanticType._
import ExprSemantic._
import ValueSemantic._
import SymbolObject._
import SymbolObjectType._
import Errors._
import scala.collection.mutable.ListBuffer
import SemanticErrorBuilder._

object StatSemantic {
  def checkStat(
      stat: Stat
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Boolean = {
    stat match {
      case Skip() =>
      /* check for declaration: what object is created? */
      case Declare(type1, ident, initValue) =>
        declareCheck(type1, ident, initValue)
      case Assign(target, newValue) => assignCheck(target, newValue)
      case Read(lvalue)             => readCheck(lvalue)
      case Free(expr)               => freeCheck(expr)
      case Return(expr)             => returnCheck(expr)
      case Exit(expr)               => exitCheck(expr)
      case Print(expr)              => checkExpr(expr)
      case Println(expr)            => checkExpr(expr)
      case If(expr, stat1, stat2)   => ifCheck(expr, stat1, stat2)
      case While(expr, stat)        => whileCheck(expr, stat)
      case Begin(stat)              => beginCheck(stat)
    }
    true
  }

  /* Name must not clash keywords and other variables in scope
     Initial value type should match type of variable*/
  def declareCheck(type1: Types.Type, ident: Ident, initValue: Rvalue)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Unit = {
    val targetType: Type = convertType(type1)
    val valueType: Type = checkRvalue(initValue)
    /* Check existence, Create new VariableObj */
    st.lookUp(ident.name, VariableType()) match {
      case Some(VariableObj(_, pos)) =>
        semErr += buildVarRedefError(
          ident.pos,
          ident.name,
          pos,
          Seq(s"Illegal redeclaration of parameter ${ident.name}")
        )
      case Some(ParamObj(_, _)) => {
        st.remove(ident.name, VariableType())
        st.add(ident.name, VariableType(), VariableObj(targetType, ident.pos))
      }
      case _ =>
        st.add(ident.name, VariableType(), VariableObj(targetType, ident.pos))
    }

    /* if intial value is nested pair, pass */
    initValue match {
      case PairElem(_, PairElem(_, _)) =>
      case _ => /* Initial value should match the type */
        if (!equalType(targetType, valueType)) {
          semErr += buildTypeError(
            ident.pos,
            valueType,
            Set(targetType),
            Seq("Value type should match variable type")
          )
        }
    }

  }

  def assignCheck(target: Lvalue, newValue: Rvalue)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Unit = {
    newValue match {
      case newValue: PairElem => pairCheck(target, newValue)
      case _ => retCheck(target, newValue)
    }
  }

  /* Target type match newValue type */
  def retCheck(target: Lvalue, newValue: Rvalue)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Unit = {
    val targetType = checkLvalue(target)
    val assignType = checkRvalue(newValue)
    if (!equalType(targetType, assignType)) {
      semErr += buildTypeError(
        newValue.pos,
        assignType,
        Set(targetType),
        Seq("Value type should match variable type")
      )
    }
  }

  def pairCheck(target: Lvalue, newValue: Rvalue)(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Unit = {
    var firstNested = false
    var secondNested = false

    // left side nested pair
    target match {
      case PairElem(_, PairElem(_, _)) => {
        checkRvalue(newValue)
        firstNested = true
      }
      case _ =>
    }

    // right side nested pair
    newValue match {
      case PairElem(_, PairElem(_, _)) => {
        checkLvalue(target)
        secondNested = true
      }
      case _ =>
    }

    // cannot be both nested
    if (firstNested && secondNested) {
      semErr += buildPairExchangeError(newValue.pos)
    }

  }

  /* Special assignment:
     Input from standard input: String
     Target type: Int / Char */
  def readCheck(
      target: Lvalue
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
    val targetType = checkLvalue(target)
    targetType match {
      case IntType()  =>
      case CharType() =>
      case _ =>
        semErr += buildTypeError(
          target.pos,
          targetType,
          Set(IntType(), CharType()),
          Seq("Only char or int can be read")
        )
    }
  }

  /* Target type: pair / array
     free is not recursive */
  def freeCheck(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
    val targetType = checkExpr(expr)
    targetType match {
      case PairType(_, _) =>
      case ArrayType(_)   =>
      case _ =>
        semErr += buildTypeError(
          expr.pos,
          targetType,
          Set(PairType(AnyType(), AnyType()), ArrayType(AnyType())),
          Seq("Only pair or array can be freed")
        )
    }
  }

  /* Need be in a non-main function
     Expr type should match return type of function */
  def returnCheck(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
    val returnType = findFuncRetType(expr.pos, st, semErr)
    val targetType = checkExpr(expr)

    if (!equalType(returnType, targetType)) {
      semErr += buildTypeError(
        expr.pos,
        targetType,
        Set(returnType),
        Seq("Return type should match target type")
      )
    }
  }

  /* Find the return type of the function in scope of st */
  def findFuncRetType(
      pos: (Int, Int),
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Type = {
    var retType: Type = null
    var recurse_st: SymbolTable = st

    while (recurse_st.encSymTable != null) {
      retType = findOneRet(recurse_st)

      if (retType != null) {
        return retType
      } else {
        recurse_st = recurse_st.encSymTable
      }
    }
    semErr += buildReturnPlacementError(
      pos,
      Seq("Return outside of function is not allowed")
    )
    return AnyType()
  }

  /* Recursive return finding */
  def findOneRet(st: SymbolTable): Type = {
    var retType: Type = null

    for ((name, obj) <- st.dictionary) {
      /* find function in this scope */
      obj match {
        case obj: FuncObj => retType = obj.returnType
        case _            =>
      }
    }

    retType
  }

  /* Can be in body of any function
     Arg type: Int */
  def exitCheck(
      expr: Expr
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
    val argType = checkExpr(expr)
    if (!equalType(argType, IntType())) {
      semErr += buildTypeError(
        expr.pos,
        argType,
        Set(IntType()),
        Seq("Exit code should be int")
      )
    }
  }

  /* Expr type: Bool
     Check validity of stat1, stat2 */
  def ifCheck(expr: Expr, stat1: List[Stat], stat2: List[Stat])(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Unit = {
    val condType = checkExpr(expr)
    if (!equalType(condType, BoolType())) {
      semErr += buildTypeError(
        expr.pos,
        condType,
        Set(BoolType()),
        Seq("Condition should be bool")
      )
    }

    /* Create new scope for each if body */
    val new_st1 = new SymbolTable(st)
    stat1.foreach { s => checkStat(s)(new_st1, semErr) }

    val new_st2 = new SymbolTable(st)
    stat2.foreach { s => checkStat(s)(new_st2, semErr) }

    /* Add new symbol table to st's subSt */
    st.addSubSt(new_st1)
		st.addSubSt(new_st2)
  }

  /* Expr type: Bool
     Check validity of stat */
  def whileCheck(expr: Expr, stat: List[Stat])(implicit
      st: SymbolTable,
      semErr: ListBuffer[WACCError]
  ): Unit = {
    val condType = checkExpr(expr)
    if (!equalType(condType, BoolType())) {
      semErr += buildTypeError(
        expr.pos,
        condType,
        Set(BoolType()),
        Seq("Condition should be bool")
      )
    }

    val new_st = new SymbolTable(st)
    stat.foreach { s => checkStat(s)(new_st, semErr) }

    /* Add new symbol table to st's subSt */
    st.addSubSt(new_st)
  }

  /* Check validity of stat
     Start new scope */
  def beginCheck(
      stat: List[Stat]
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
    val new_st = new SymbolTable(st)
    stat.foreach { s => checkStat(s)(new_st, semErr) }

    /* Add new symbol table to st's subSt */
    st.addSubSt(new_st)
  }

}
