package subscript

import subscript.language

import subscript.vm._
import subscript.vm.model.callgraph._

import scala.util.Try

/** Contains DSL scripts. */
object ScriptDSL {

  def _dataflow(s: Script[Any], t: Any => Script[Any], e: Throwable => Script[Any]): Script[Any] = {
    var s_node: N_call[Any] = null
    ([do @{s_node = there.asInstanceOf[N_call[Any]]}: s then t(s_node.$success)^ else e(s_node.$failure)^])
  }

  script..
    _dataflow_map[T](s: Script[Any], f: T => Any) =
      @{there.onSuccess {`script`.$ = there.asInstanceOf[ScriptResultHolder[Any]].$.map(r => f(r.asInstanceOf[T]))} }: s

}
