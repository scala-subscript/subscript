package subscript

import subscript.language

import subscript.vm._

import scala.util.Try

/** Contains DSL scripts. */
object ScriptDSL {

    // we do not want scripts in DSL.scala, because a regular Scala compiler should be able to translate this. 
  // So we manually translate the following script:
  //
  //def script dataflow_then[T,U](s: Script[T], t: T=>Script[U]): U = 
  //     var s_node: N_call[T] = null;
  //     do @{s_node = there}: s then t(s_node.callee.$.get)
  
  // Second parameter not t but _t; SubScript code generation quirk, should be undone.
  //def script dataflow_then[T](s: Script[T], _t: T=>ScriptNode[Any]): Any = 
  //     var s_node: N_call[Any] = null;
  //     do @{s_node = there}: s then t(s_node.callee.$.get.asInstanceOf[T])
  //def script dataflow_then(s: Script[Any], t: Any=>Script[Any]): Any = 
  //     var s_node: N_call[Any] = null;
  //     do @{s_node = there}: s then t(s_node.callee.$.get)
  

  // def _dataflow_then[S,T](s: ScriptNode[S], t: S=>ScriptNode[T]): T_do_then = 
  //   {
  //      var s_node: N_call[S] = null
  //      //println(" _dataflow_then >> INIT")
  //      _do_then(_call("do",   (_n:N_call[S]) => {//println(" _dataflow_then >> _do_then do-part"); 
  //                                             s_node = _n; _n.calls(s.template, (s.p: _*)); s}), 
  //               _call("then", (_n:N_call[T]) => {//println(" _dataflow_then >> _do_then then-part: ${s_node.callee.$.get}"); 
  //                                             //println(s"s_node.callee.$$: ${s_node.callee.$}")
  //                                             val tp = t(s_node.$.get); 
  //                                             _n.calls(tp.template, (tp.p: _*)); tp}, true))
  //   }

  script..
    _dataflow(s: ScriptNode[Any], t: Any => ScriptNode[Any], e: Throwable => ScriptNode[Any]) =
      var s_node: N_call[Any] = null
      do @{s_node = there.asInstanceOf[N_call[Any]]}: s then t(s_node.$success) else e(s_node.$failure)


}