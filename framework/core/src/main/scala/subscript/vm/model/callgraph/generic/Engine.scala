package subscript.vm.model.callgraph.generic

import subscript.vm.Script
import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._

import subscript.vm.{CodeExecutorTrait, CodeExecutorAdapter}

trait Engine {this: CallGraphNode =>
  var _scriptExecutor: ScriptExecutor[_] = null
  def scriptExecutor = _scriptExecutor
  def scriptExecutor_=(s: ScriptExecutor[_]) = {
    index = s.graph.nextNodeIndex
    _scriptExecutor = s
  }
  
  var codeExecutor: CodeExecutorTrait = null
  
  def launch(aScript: Script[_]) = {_scriptExecutor.launch(this, aScript)}
}

object Engine {
  def adaptExecutor(e: Engine, ca: CodeExecutorAdapter[_,CodeExecutorTrait]) {
    ca.adapt(e.codeExecutor)
    e.codeExecutor=ca
  }
}
