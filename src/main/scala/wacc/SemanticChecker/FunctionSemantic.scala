package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer

import wacc.Error.Errors._
import wacc.Error.SemanticErrorBuilder._
import wacc.Ast._

import SymbolObject._
import SymbolObjectType._
import StatSemantic._
import SemanticTypes.{convertType}

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
    st.lookUp(func.ident.name, FunctionType()) match {
      /* Function redefinition */
      case Some(obj) => {
        semErr += buildFuncRedefError(
          func.ident.pos,
          func.ident.name,
          obj.getPos(),
          Seq(s"Illegal redeclaration of parameter ${func.ident.name} ")
        )
      }
      case None => {
        /* add function to main scope */
        st.add(
          func.ident.name,
          FunctionType(),
          new FuncObj(
            convertType(func.type1),
            args.toList,
            func.params.length,
            st,
            func.pos
          )
        )
      }
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
      new_st.lookUp(p.ident.name, VariableType()) match {
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
