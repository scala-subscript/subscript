// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._

case class N_localvar[V](template: T_localvar[V]) extends CallGraphLeafNode
{
  type T = T_localvar[V]
  
  override def passToBeUsedToGetVariableNamed(thatName: Symbol): Int =
    if (template.isLoop&&template.localVariable.name==thatName)
      pass-1 else pass // used in: var i=0...(i+1)
}

case class N_privatevar(template: T_privatevar)
  extends CallGraphNode
  with    GraphLeafNode
  {type T = T_privatevar}
