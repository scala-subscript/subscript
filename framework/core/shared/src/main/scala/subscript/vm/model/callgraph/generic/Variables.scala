package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._
import subscript.vm.VariableHolder
import scala.collection.mutable.HashMap

trait Variables {this: CallGraphNode => // TBD: change CallGraphNode into CallGraphTreeNode
  
  /** Pass flag; should possibly move elsewhere */
  var pass = 0
  
  
  def passToBeUsedToGetVariableNamed(name: Symbol) = pass
  def getLocalVariableHolder[V<:Any](name: Symbol): VariableHolder[V] = {
    //var usePass = this match {
    //  case lvl@N_localvar_loop(_:T) if (lvl.name==name) => pass-1
    //  case _ => pass
    //}
    // yields strange compile error message:
    // constructor cannot be instantiated to expected type;  found   : subscript.vm.N_localvar_loop[V]  required: subscript.vm.CallGraphTreeNode[T] 
    // so the following code is used instead:

    var usePass = passToBeUsedToGetVariableNamed(name)
    var result: VariableHolder[V] = null
    var nary   = n_ary_op_ancestor
    while (result==null) {
      result = nary.getVariableHolder(name, usePass)
      if (result==null) {
        usePass = nary.pass
        nary    = nary.n_ary_op_ancestor
      }
    }
    result
  }
}

trait VariablesContainer extends Variables {this: CallGraphTreeNode =>
  val mapNamePassToVariableHolder = new HashMap[(Symbol,Int), VariableHolder[_]]
  
  def initLocalVariable[V<:Any](name: Symbol, fromPass: Int, value: V) =
    mapNamePassToVariableHolder += ((name,fromPass)->new VariableHolder(value))
 
  def getVariableHolder[V<:Any](name: Symbol, fromPass: Int):VariableHolder[V] =
    mapNamePassToVariableHolder.get((name,fromPass)) match {
      case None                      => null 
      case Some(v:VariableHolder[V]) => v 
   }
}
