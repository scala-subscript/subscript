package subscript.vm.model.callgraph.generic

import scala.collection.mutable.HashMap
import subscript.vm.model.callgraph.CallGraphNode

trait Container {this: CallGraphNode =>
  private[this] val properties = new HashMap[Any,Any]
  
  def getCodeProperty(key: Any): ()=>Unit = {
    properties.get(key) match {
      case None => null
      case Some(cp) => cp.asInstanceOf[()=>Unit]
    }
  }
  
  def setCodeProperty(key: Any, c: ()=>Unit) = {
    properties += key->c
  }
}