package subscript

import subscript.language

import subscript.vm._

import scala.util.Try

/** Contains DSL scripts. */
object ScriptDSL {

  script..
    _dataflow(s: ScriptNode[Any], t: Any => ScriptNode[Any], e: Throwable => ScriptNode[Any]) =
      var s_node: N_call[Any] = null
      do @{s_node = there.asInstanceOf[N_call[Any]]}: s then t(s_node.$success) else e(s_node.$failure)


}