package subscript.vm.model.callgraph

import scala.collection.mutable.ListBuffer

/**
 * A graph node. It can have parents of type Parent and children
 * of type Child.
 */
trait GraphNode {
  type Child  <: GraphNode
  type Parent <: GraphNode
    
  def children: Seq[Child ] = _children
  def parents : Seq[Parent] = _parents
  
  private val _children = new ListBuffer[Child ]
  private val _parents  = new ListBuffer[Parent]
  
  /**
   * Links a new parent to this node. This will result
   * to this node being registered as a child within
   * the argument.
   */
  def addParent(p: Parent) {p addChild this.asInstanceOf[p.Child]}
  
  /**
   * Unlinks the parent.
   */
  def removeParent(p: Parent) {if (p!=null)/*test should not be needed*/ p removeChild this.asInstanceOf[p.Child]}
  
  /**
   * Links a new child.
   */
  def addChild(c: Child) {
    _children  += c
    c._parents += this.asInstanceOf[c.Parent]
  }
  
  def removeFrom[T<:AnyRef](lb: ListBuffer[T], elt: T) {
    // lb -= elt    
    // not applicable for nodes since these are case classes with a too generous Equals method.
    // use eq() instead
    val indexToRemove = lb.indexWhere(_.eq(elt))
    lb.remove(indexToRemove)
  }
  /**
   * Unlinks the child.
   */
  def removeChild(c: Child) {
    //_children -= c
    //c._parents -= this.asInstanceOf[c.Parent]
    removeFrom(_children , c)
    removeFrom(c._parents, this.asInstanceOf[c.Parent])
  }
}

/**
 * Graph node with only one parent. Any attempt to add a second parent
 * will result in current parent being removed.
 */
trait GraphTreeNode extends GraphNode {
  def parent: Parent = parents.headOption getOrElse null.asInstanceOf[Parent]
  
  override def addParent(p: Parent) {
    if (parent ne null) removeParent(parent)
    super.addParent(p)
  }
}

/**
 * Graph tree node that can't have children.
 */
trait GraphLeafNode extends GraphTreeNode {
  override def addChild(c: Child) = throw new RuntimeException("This node can't have children")
}