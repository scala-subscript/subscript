package subscript.swing
import subscript.file

import subscript.swing.Scripts._
import subscript.DSL._

import scala.language.implicitConversions

object SubScriptDebugger extends SubScriptDebuggerApp
object SubScriptDebugger2 extends SubScriptDebuggerApp {
  // extra singleton to allow for GraphicalDebugging the SubScriptDebuggerApp
  override def doesThisAllowToBeDebugged = true
}

class SubScriptDebuggerApp extends SimpleSubscriptApplication with GraphicalDebugger {

  override def live: Unit = try _execute(liveScript, debugger=null, myScriptExecutor)
                            catch {case t:Throwable => t.printStackTrace; throw t}

  override def main(args: Array[String]) = super.main(args)

def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._seq(subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("awaitMessageBeingHandled(subscript.DSL._maybeVarCall(\"true\"))")
}, true), subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("shouldStep")
})(subscript.DSL._seq(subscript.DSL._at[subscript.vm.N_code_tiny[Any], subscript.vm.model.template.concrete.T_code_tiny[Any]](here => {
  implicit val there: subscript.vm.N_code_tiny[Any] = here.there;
gui
}).apply(subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("updateDisplay")
}, true)), subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => stepCommand), subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("autoCheckBox.selected")
})(subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("waitForStepTimeout")
}, true))))), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("messageBeingHandled(subscript.DSL._maybeVarCall(\"false\"))")
}, true), subscript.DSL._loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitDebugger))}
def stepCommand = subscript.DSL._script[Any](None, Symbol("stepCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => stepButton)}
def exitCommand = subscript.DSL._script[Any](None, Symbol("exitCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitButton)}
def exitDebugger = subscript.DSL._script[Any](None, Symbol("exitDebugger")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitCommand), subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"exitConfirmed\",subscript.DSL._maybeVarCall(\"confirmExit\"))")
}, true)), subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("!exitConfirmed")
}))}

}