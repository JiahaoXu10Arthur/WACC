package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer

import wacc.Error.Errors._
import wacc.Error.SemanticErrorBuilder._
import wacc.Ast._

import SymbolObject._
import SymbolObjectType._
import FunctionSemantic._
import StructSemantic._
import SemanticTypes.{convertType}

object ClassSemantic {
  /* Load only header into main scope */
  def readInClassHeader(
    waccClass: Class
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
	/* Check for class redefinition */
    /* Class cannot have same name as struct */
    val classIdent = waccClass.struct.name
    st.lookUpAll(classIdent.name, StructObjType()) match {
      case Some(obj) => {
        semErr += buildClassRedefError(
          classIdent.pos,
          classIdent.name,
          obj.getPos(),
          Seq(s"Illegal redeclaration of class ${classIdent.name} of other struct")
        )
      }
      case None =>
  	}

    /* Class cannot have same name among classes */
    st.lookUpAll(classIdent.name, ClassObjType()) match {
      case Some(obj) => {
        semErr += buildClassRedefError(
          classIdent.pos,
          classIdent.name,
          obj.getPos(),
          Seq(s"Illegal redeclaration of class ${classIdent.name} of other class")
        )
      }
      // Add class to overall scope
      case None => {
        st.add(
          classIdent.name,
          ClassObjType(),
          ClassObj(
            classIdent,
			waccClass.struct,
			st,
            waccClass.pos
          )
        )
      }
  	}
  }

  /* Create self symbol table */
  def checkClassDeclaration(
      waccClass: Class
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
	
    // Create class symbol table
    val classSt: SymbolTable = new SymbolTable(st, VariableType())

    val struct = waccClass.struct
    // Read in struct - do not need to check for redefinition again
		classSt.add(
          struct.name.name,
          StructObjType(),
          StructObj(
					struct.name,
          struct.fields.map(f => (f._2, VariableObj(convertType(f._1), f._2.pos))),
					classSt,
          struct.pos))

		val args = new ListBuffer[(Ident, VariableObj)]()
		/* Add obj into self scope */
		struct.fields.foreach { 
			case (t, i) => {
				classSt.lookUp(i.name, VariableType()) match {
					case Some(obj) => {
						semErr += buildStructFieldRedefError(
							i.pos,
							i.name,
							obj.getPos(),
							Seq(s"Illegal redeclaration of field ${i.name} ")
						)
					}
					/* Add parameter to scope */
					case None => {
						classSt.add(
							i.name,
							VariableType(),
							VariableObj(convertType(t), i.pos)
						)
					}
					args += ((i, VariableObj(convertType(t), i.pos)))
				}
			}
		}

    // Read in function
    waccClass.funcs.foreach(readInFunctionHeader(_)(classSt, semErr))
    waccClass.funcs.foreach(checkFuncDeclare(_)(classSt, semErr))

    // Link class symbol table to overall symbol table
    st.addSubSt(classSt)

    // Link class symbol table to class AST node
    waccClass.symb = classSt

    // Link symbol table to ClassObj
		st.lookUpAll(waccClass.struct.name.name, ClassObjType()) match {
			case Some(obj: ClassObj) => {
				obj.symTable = classSt
			}
			case _ => 
		}
  }
}
