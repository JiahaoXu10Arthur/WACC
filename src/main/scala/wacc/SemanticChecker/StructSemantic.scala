package wacc.SemanticChecker

import scala.collection.mutable.ListBuffer

import wacc.Error.Errors._
import wacc.Error.SemanticErrorBuilder._
import wacc.Ast._

import SymbolObject._
import SymbolObjectType._
import SemanticTypes.{convertType}

object StructSemantic {
  /* Load only header into main scope */
  def readInStructHeader(
    struct: Struct
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
	/* Check for struct redefinition */
    st.lookUpAll(struct.name.name, StructObjType()) match {
      case Some(obj) => {
        semErr += buildStructRedefError(
          struct.name.pos,
          struct.name.name,
          obj.getPos(),
          Seq(s"Illegal redeclaration of struct ${struct.name.name} ")
        )
      }
	/* add struct to main scope */
      case None => {
        st.add(
          struct.name.name,
          StructObjType(),
          StructObj(
			struct.name,
            struct.fields.map(f => (f._2, VariableObj(convertType(f._1), f._2.pos))),
			st,
            struct.pos
          )
        )
      }
  	}
  }

  /* Create self symbol table
			 Add obj into self scope */
  def checkStructDeclare(
      struct: Struct
  )(implicit st: SymbolTable, semErr: ListBuffer[WACCError]): Unit = {
		/* Create new symbol table */
		val new_st = new SymbolTable(st, StructObjType())
		val args = new ListBuffer[(Ident, VariableObj)]()
		/* Add obj into self scope */
		struct.fields.foreach { 
			case (t, i) => {
				new_st.lookUp(i.name, VariableType()) match {
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
						new_st.add(
							i.name,
							VariableType(),
							VariableObj(convertType(t), i.pos)
						)
					}
					args += ((i, VariableObj(convertType(t), i.pos)))
				}
			}
		}

		/* Add new symbol table to st's subSt */
    st.addSubSt(new_st)

    /* Add symbol table to struct */
    struct.symb = new_st

		/* link symbol table to structObj */
		st.lookUpAll(struct.name.name, StructObjType()) match {
			case Some(obj: StructObj) => {
				obj.symTable = new_st
			}
			case _ => 
		}
  }
}
