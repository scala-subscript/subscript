package subscript.twitter.app.controller
import subscript.file

import subscript.DSL._
import subscript.swing.Scripts._

import subscript.twitter.api._
import subscript.twitter.app.view.View

/**
 * Created by anatolii on 11/28/14.
 */
class SubScriptController(val view: View) extends Controller {

  def start() = {
      val executor = new subscript.vm.executor.CommonScriptExecutor[Any]
      val debugger = new subscript.vm.SimpleScriptDebuggerClass
      executor.traceLevel = 2
      debugger.traceLevel = 4
      _execute(liveScript)
      //_execute(_live(), debugger, executor)
  }
  //def start() = _execute(_live())

  def sleep(t: Long) = Thread sleep t

def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => initialize), subscript.DSL._seq(subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mainSequence), subscript.DSL._optionalBreak_loop), subscript.DSL._loop))}
def initialize = subscript.DSL._script[Any](None, Symbol("initialize")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => view.main(subscript.DSL._maybeVarCall("Array()")))}
def mainSequence = subscript.DSL._script[Any](None, Symbol("mainSequence")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => anyEvent(subscript.DSL._maybeVarCall("view.searchField"))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => waitForDelay), subscript.DSL._dataflow_then_else(
  subscript.DSL._script[Any](None, Symbol("~~>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => searchTweets)}
, (_ts: Any) => _ts match {case ts: Seq[Tweet] => subscript.DSL._script[Any](None, Symbol("<lambda>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => updateTweetsView(subscript.DSL._maybeVarCall("ts")))}}
, (_t: Any) => _t match {case t: Throwable => subscript.DSL._script[Any](None, Symbol("<lambda>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setErrorMsg(subscript.DSL._maybeVarCall("t")))}}
))}
def waitForDelay = subscript.DSL._script[Any](None, Symbol("waitForDelay")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("view.setStatus(subscript.DSL._maybeVarCall(\"\\\"waiting\\\"\")  )"); subscript.DSL._maybeVarCall("sleep(subscript.DSL._maybeVarCall(\"keyTypeDelay\"))")
}, true)}
def searchTweets = subscript.DSL._script[Any](None, Symbol("searchTweets")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("view.setStatus(subscript.DSL._maybeVarCall(\"\\\"searching\\\"\"))"); subscript.DSL._maybeVarCall("twitter.search(subscript.DSL._maybeVarCall(\"view.searchField.text\"), subscript.DSL._maybeVarCall(\"tweetsCount\"))")
}, true)}
def updateTweetsView(ts: Seq[Tweet]) = subscript.DSL._script[Any](None, Symbol("updateTweetsView")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("view.setTweets(subscript.DSL._maybeVarCall(\"ts\"))")
}, true))}
def setErrorMsg(t: Throwable) = subscript.DSL._script[Any](None, Symbol("setErrorMsg")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("view.setError(subscript.DSL._maybeVarCall(\"t.toString\"))")
}, true))}
}
