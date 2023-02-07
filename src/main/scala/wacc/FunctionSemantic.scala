package wacc

import Ast._
import SymbolObject._
import SymbolObjectType._
import StatSemantic._
import StatSemantic.{convertType}
import Errors._
import SemanticErrorBuilder._
import scala.collection.mutable.ListBuffer


object FunctionSemantic {
		/* Load only header into main scope */
		def readInFunctionHeader(func: Func)
														(implicit st: SymbolTable, 
																			semErr: ListBuffer[WACCError]): Unit = {
			val args = new ListBuffer[ParamObj]()
			func.params.foreach {p => 
				args += new ParamObj(convertType(p.paramType), p.pos)
			}

			st.lookUp(func.ident.name, FunctionType()) match {
			case Some(obj) => {
				semErr += buildFuncRedefError(None, func.ident.pos, func.ident.name, 
																			obj.getPos(), Seq("Function: parameter already defined"), "")
			}
			case None => {
				/* add func to main scope */
				st.add(func.ident.name, FunctionType(), 
							 new FuncObj(convertType(func.type1), 
							 						 args.toList, func.params.length, st, func.pos))
			}
		}

			st.add(func.ident.name, FunctionType(), new FuncObj(convertType(func.type1), 
						 args.toList, func.params.length, st, func.pos))
		}

		/* Create self symbol table
			 Add obj into self scope
			 Check statements */
		def checkFuncDeclare(func: Func)
												(implicit st: SymbolTable, 
																	semErr: ListBuffer[WACCError]): Unit = {
			/* Create new symbol table */
    	val new_st = new SymbolTable(st)
			val args = new ListBuffer[ParamObj]()

			func.params.foreach {p => 
				new_st.lookUp(p.ident.name, VariableType()) match {
					case Some(obj) => {
						semErr += buildParamRedefError(None, p.ident.pos, p.ident.name, 
																					 obj.getPos(), Seq("Function: parameter already defined"), "")
						}
					case None => {
						new_st.add(p.ident.name, VariableType(), new ParamObj(convertType(p.paramType), p.pos))
						args += new ParamObj(convertType(p.paramType), p.pos)
					}
				}
			}

			/* add func to its self scope */
			new_st.add(func.ident.name, FunctionType(), new FuncObj(convertType(func.type1), 
						 		 args.toList, args.length, new_st, func.pos))

			func.stats.foreach(s => checkStat(s)(new_st, semErr))
		}
}
