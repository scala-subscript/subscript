package subscript
import subscript.file

// import org.junit.runner.RunWith

import subscript.Predef._
import subscript.DSL._
import subscript.vm.{N_code_unsure, SimpleScriptDebugger, Script}
import subscript.vm.model.callgraph._
import subscript.vm.executor._

class TestC {
    
def times(n: Int) = subscript.DSL._script[Any](None, Symbol("times")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.pass < n")
})}
def key(c: subscript.vm.FormalConstrainedParameter[Char]) = subscript.DSL._script[Any](None, Symbol("key"), c.~??(Symbol("c"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"c\",subscript.DSL._maybeVarCall(\"'!'\"))")
}, true)}
def mainScript(args: Array[String]) = {
  val c = subscript.DSL._declare[Char](scala.Symbol("c"))
  subscript.DSL._script[Any](None, Symbol("mainScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._val(c, (_node: subscript.vm.N_localvar[Char]) => {implicit val here = _node; val tr: Char = subscript.DSL._maybeVarCall("' '"); tr}), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => key(subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"c\")"))), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("println(subscript.DSL._maybeVarCall(\"s\\\"key =>  $c\\\"\"))")
}, true)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => key(subscript.DSL._maybeVarCall("'!'"))), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("println(subscript.DSL._maybeVarCall(\"\\\"key <= '!'\\\"\"))")
}, true)))}
  
}

  val times1: Int=>Script[Any] = n=>{_script(this, 'lambda) {script => _while{implicit here=>pass<n}}}
  val times2: Int=>Script[Any] = n=>subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.pass < n")
})}
  //val times3: Int=>Script =     [ while(pass < _) ]
  //val noXml = [noXML]

  //TBD: allow ? in all parameter lists; parse < script > blocks
}
object Test extends TestC {
  // bridge method:
  def main( args: Array[String]): Unit = _execute(mainScript(args))
}
