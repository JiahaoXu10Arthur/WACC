package wacc

class SymbolTable(st: SymbolTable) {

  val dictionary = collection.mutable.Map[String, Object]()
  val encSymTable = st

  def add(name: String, obj: Object)
    = dictionary += (name -> obj)

  def lookUp(name: String)
    = dictionary.get(name)

  // Lookup name in current and enclosing
  def lookUpAll(name: String): Option[Object] = {
    var s = this
    while (s != null) {
        val obj = s.lookUp(name)
        s = s.encSymTable
        if (obj != None) {
            obj
        }
    }

    None
  }
}
