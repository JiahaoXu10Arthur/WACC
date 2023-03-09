package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer

import wacc.Error.Errors._
import wacc.Error.SemanticErrorBuilder._
import wacc.Ast._

import SymbolObject._
import SymbolObjectType._
import StatSemantic._
import SemanticTypes._

object FunctionSemantic {
  /* Load only header into main scope */
  def readInFunctionHeader(
      func: Func
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {

    /* Find each parameter */
    val args = new ListBuffer[ParamObj]()
    func.params.foreach { p =>
      args += new ParamObj(convertType(p.paramType), p.pos)
    }

    /* Check for function redefinition */
    val expectedRet  = convertType(func.type1)
    val expectedArgs = args.map(_.t).toList

    var redefFunc = false
    st.lookUpFunc(func.ident.name) match {
      /* Function redefinition */
      case Some(objs) => {
        objs.foreach {
          case obj: FuncObj => {
              val objRet  = obj.returnType
              val objArgs = obj.args.map(_.t)
              /* All arguments are the same type, function redefinition */
              if (sameFunction(expectedRet, objRet, expectedArgs, objArgs)) {
                semErr += buildFuncRedefError(
                  func.ident.pos,
                  func.ident.name,
                  obj.getPos(),
                  Seq(s"Illegal redeclaration of parameter ${func.ident.name} ")
                )
                redefFunc = true
              }
            }
          case _ =>
        }
      }
      case None =>
    }

    /* Add function definition to main scope */
    if (!redefFunc) {
      st.add(
        func.ident.name,
        FunctionType(),
        new FuncObj(
          expectedRet,
          args.toList,
          args.length,
          st,
          func.pos
        )
      )
    }

  }

  /* Create self symbol table
			 Add obj into self scope
			 Check statements */
  def checkFuncDeclare(
      func: Func
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
    /* Create new symbol table */
    val new_st = new SymbolTable(st, FunctionType())
    val args = new ListBuffer[ParamObj]()

    /* Check for parameter redefinition */
    func.params.foreach { p =>
      new_st.lookUpVar(p.ident.name) match {
        /* Parameter redefinition */
        case Some(obj) => {
          semErr += buildParamRedefError(
            p.ident.pos,
            p.ident.name,
            obj.getPos(),
            Seq(s"Illegal redeclaration of parameter ${p.ident.name} ")
          )
        }
        /* Add parameter to scope */
        case None => {
          new_st.add(
            p.ident.name,
            VariableType(),
            new ParamObj(convertType(p.paramType), p.pos)
          )
          args += new ParamObj(convertType(p.paramType), p.pos)
        }
      }
    }

    /* Add function definition to its self scope */
    new_st.add(
      func.ident.name,
      FunctionType(),
      new FuncObj(
        convertType(func.type1),
        args.toList,
        args.length,
        new_st,
        func.pos
      )
    )

    /* Check body of function */
    func.stats.foreach(s => checkStat(s)(new_st, semErr))

    /* Add new symbol table to st's subSt */
    st.addSubSt(new_st)

    /* Add symbol table to func */
    func.symb = new_st
  }
}
