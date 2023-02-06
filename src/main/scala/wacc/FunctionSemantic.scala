package wacc

import Ast._
import SymbolObject._
import StatSemantic._
import StatSemantic.{convertType}

object FunctionSemantic {
		def checkFuncDeclare(func: Func, st: SymbolTable): Unit = {
			/* Create new symbol table */
    	val new_st = new SymbolTable(st)
			val args: Array[VariableObj] = new Array[VariableObj](func.params.length)

			func.params.foreach {p => 
				st.add(p.ident.name, new VariableObj(convertType(p.paramType)))
				args :+ new VariableObj(convertType(p.paramType))
			}

			/* add func to main scope */
			st.add(func.ident.name, new FuncObj(convertType(func.type1), 
						 args.toList, args.length, st))

			/* add func to its self scope */
			st.add(func.ident.name, new FuncObj(convertType(func.type1), 
						 args.toList, args.length, new_st))

			func.stats.foreach(s => checkStat(s, new_st))
		}
}
