package subscript.twitter.app.controller
import subscript.file

import subscript.DSL._
import subscript.swing.Scripts._

import subscript.twitter.api._
import subscript.twitter.app.view.View

import subscript.twitter.util.{InterruptableFuture, CancelException}
import subscript.twitter.util.InterruptableFuture.Implicits.executionContext
import scala.util.{Success, Failure}

/**
 * Created by andre on 2/22/15.
 */
class SubScriptFuturesController(val view: View) extends Controller {
  import scala.language.implicitConversions

  def startDebug() = {
      val executor = new subscript.vm.executor.CommonScriptExecutor[Any]
      val debugger = new subscript.vm.SimpleScriptDebuggerClass
      executor.traceLevel = 2
      debugger.traceLevel = 4
      _execute(liveScript, debugger, executor)
  }
  def start() = _execute(liveScript)
  
  val fWait   = InterruptableFuture {Thread sleep keyTypeDelay}
  val fSearch = InterruptableFuture {twitter.search(view.searchField.text, tweetsCount)}
  val faulter = InterruptableFuture {throw new Exception("Never faulter")}

  implicit def f2s(intf: InterruptableFuture[_]): subscript.vm.Script[Any] = subscript.DSL._script[Any](None, Symbol("f2s")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_eventhandling[Any], subscript.vm.model.template.concrete.T_code_eventhandling[Any]](here => {
  implicit val there: subscript.vm.N_code_eventhandling[Any] = here.there;
subscript.DSL._maybeVarCall("intf.execute().onComplete {case aTry => subscript.DSL._maybeVarCall(\"there.executeForTry(subscript.DSL._maybeVarCall(\\\"aTry\\\"))\")}")
}).apply(subscript.DSL._eventhandling[Any] (_node => {
  implicit val here = _node

}, true))}

def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => initialize), subscript.DSL._seq(subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mainSequence), subscript.DSL._optionalBreak_loop), subscript.DSL._loop))}
def initialize = subscript.DSL._script[Any](None, Symbol("initialize")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => view.main(subscript.DSL._maybeVarCall("Array[String]()")))}
def mainSequence = subscript.DSL._script[Any](None, Symbol("mainSequence")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => anyEvent(subscript.DSL._maybeVarCall("view.searchField"))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => fWait), subscript.DSL._dataflow_then_else(
  subscript.DSL._script[Any](None, Symbol("~~>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => fSearch)}
, (_ts: Any) => _ts match {case ts: Seq[Tweet] => subscript.DSL._script[Any](None, Symbol("<lambda>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => updateTweetsView(subscript.DSL._maybeVarCall("ts")))}}
, (_t: Any) => _t match {case t: Throwable => subscript.DSL._script[Any](None, Symbol("<lambda>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setErrorMsg(subscript.DSL._maybeVarCall("t")))}}
))}
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
