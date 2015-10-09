package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._
import subscript.vm.CodeExecutorAdapter
import subscript.vm.CodeExecutorTrait

trait OldCallGraphNodeApi {this: CallGraphNode =>
  
  def forEachParent(f: Parent => Unit) = parents  foreach f
  def forEachChild (f: Child  => Unit) = children foreach f
  
  def lowestSingleCommonAncestor: CallGraphNode =
    if (parents.size < 2) parents.headOption.getOrElse(null)
    else null
    
  def adaptExecutor(ca: CodeExecutorAdapter[_,CodeExecutorTrait]): Unit =
    {ca.adapt(codeExecutor); codeExecutor=ca}
}