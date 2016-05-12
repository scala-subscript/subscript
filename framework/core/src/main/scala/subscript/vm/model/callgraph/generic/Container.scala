package subscript.vm.model.callgraph.generic

import scala.collection.mutable.HashMap
import subscript.vm.model.callgraph.CallGraphNode

trait Container {this: CallGraphNode =>
  private[this] val properties = new HashMap[Any,Any]
  
  def setProperty[K, V](k: K, v: V) {properties += k -> v}
  def getProperty[K, V](k: K): Option[V] = properties.get(k).map(_.asInstanceOf[V])
  
  def getCodeProperty(key: Any): ()=>Unit = getProperty[Any, () => Unit](key) match {
    case None => null
    case Some(cp) => cp.asInstanceOf[()=>Unit]
  }
  
  def setCodeProperty(key: Any, c: ()=>Unit) = setProperty(key, c)
}
