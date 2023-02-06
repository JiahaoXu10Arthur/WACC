package wacc

import Ast._
import SymbolObject._
import SymbolObjectType._
import StatSemantic._
import StatSemantic.{convertType}
import scala.collection.mutable.ListBuffer


object FunctionSemantic {
		/* Load only header into main scope */
		def readInFunctionHeader(func: Func, st: SymbolTable): Unit = {
			val args = new ListBuffer[ParamObj]()
			func.params.foreach {p => 
				args += new ParamObj(convertType(p.paramType))
			}

			/* add func to main scope */
			st.add(func.ident.name, FunctionType(), new FuncObj(convertType(func.type1), 
						 args.toList, func.params.length, st))
		}

		/* Create self symbol table
			 Add obj into self scope
			 Check statements */
		def checkFuncDeclare(func: Func, st: SymbolTable): Unit = {
			/* Create new symbol table */
    	val new_st = new SymbolTable(st)
			val args = new ListBuffer[ParamObj]()

			func.params.foreach {p => 
				new_st.add(p.ident.name, VariableType(), new ParamObj(convertType(p.paramType)))
				args += new ParamObj(convertType(p.paramType))
			}

			/* add func to its self scope */
			new_st.add(func.ident.name, FunctionType(), new FuncObj(convertType(func.type1), 
						 		 args.toList, args.length, new_st))

			func.stats.foreach(s => checkStat(s, new_st))
		}
}
