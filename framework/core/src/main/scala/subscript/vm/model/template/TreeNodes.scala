package subscript.vm.model.template

/**
 * Helpers for TreeNode.
 */
object TreeNode {
  
  def setIndexes(tn: TreeNode, startIndexInScript: Int, indexForChild: Int): Int = {
    tn.indexAsChild  = indexForChild
    tn.indexInScript = startIndexInScript
    var result    = 1
    var i         = 0
    tn.children.foreach{c =>
      c.parent = tn.asInstanceOf[c.Parent]; 
      result  += setIndexes(c, startIndexInScript + result, i)
      i       += 1
    }
    result
  }
  
}

/**
 * Base class for all Tree nodes.
 */
trait TreeNode {
  type Root   <: RootNode
  type Parent <: TreeNode
  type Child  <: ChildNode
  
  def root  : Root
  def parent: Parent
  
  var indexAsChild: Int = 0
  var indexInScript: Int = 0
  
  def children: Seq[Child]
}

/**
 * A non-root node.
 */
trait ChildNode extends TreeNode {
  def root  : Root   = (if (parent==null) null else parent.root).asInstanceOf[Root]
  var parent: Parent = null.asInstanceOf[Parent]
}

/**
 * A root node.
 */
trait RootNode extends TreeNode {
  def parent = null.asInstanceOf[Parent]
  def root   = this.asInstanceOf[Root]
}

/*
 * Differentiation of the nodes based on how many children do they contain.
 */
trait TreeNode_0 extends TreeNode   {
  override def children: Seq[Child] = Nil
}

trait TreeNode_1_def extends TreeNode_0 {
  def child0: Child
}

trait TreeNode_1 extends TreeNode_1_def {
  val child0: Child
  override val children: Seq[Child] = child0::Nil
}

trait TreeNode_2 extends TreeNode_1 {
  val child1: Child
  override val children: Seq[Child] = child0::child1::Nil
}

trait TreeNode_3 extends TreeNode_2 {
  val child2: Child
  override val children: Seq[Child] = child0::child1::child2::Nil
}
