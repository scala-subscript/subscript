package subscript.vm.model.callgraph

import scala.util.{Try,Success,Failure}
import subscript.vm._
import subscript.vm.model.template._
import subscript.vm.model.callgraph.generic._
import subscript.vm.CallGraphMessage

trait CallGraphNode extends GraphNode
    with Container
    with Engine
    with GraphNavigation
    with ListenableNode
    with State
    with ChildrenState
    with Informational
    with OldCallGraphNodeApi
    
    with Variables // TBD: move to CallGraphTreeNode
{
  type T <: TemplateNode
  type S
  override type Child  = CallGraphNode.Child
  override type Parent = CallGraphNode.Parent
  
  def template: T
  var index = -1
  var scriptNode: ScriptNode[_] = null
  def resultPropagationDestination[R]: ScriptResultHolder[R] = (if (scriptNode!=null) scriptNode
                                                               else                   scriptExecutor).asInstanceOf[ScriptResultHolder[R]]
  
  override def toString = f"$index%2d $template"
}
trait ScriptResultHolder[R] {
  var $:Try[R] = null; 
  def $success:R = $.get; 
  def $success_=(v:R) = $ = Success(v); 
  def $failure:Throwable = if ($==null||$.isSuccess)null else $.asInstanceOf[Failure[R]].exception
  def $failure_=(f:Throwable) = {$ = Failure[R](f); fail} //; println(s"failure: $f")}
  def fail: Unit
  
  def setResult(t: Try[_]) = {
    //println(s"node: $this setResult: $t")
    $ = t.asInstanceOf[Try[R]]
  }
  
  // TBD: remove
  def $capture(body: ()=>R) = {
    import scala.language.existentials
    try {$success = body()}
    catch{case t:Throwable => fail; $failure=t} //; println(s"captured: $t")}
  }
  def $capturer(body: =>R): ()=>Unit = () =>
    try {$success = body}
    catch {case t:Throwable => fail; $failure=t} //; println(s"captured: $t")}
  
  def resultPropagationDestination[R]: ScriptResultHolder[R]
  def propagateResult = {
    import scala.language.existentials
    //println(s"node: $this propagateResult: $result to: $resultPropagationDestination")
    resultPropagationDestination.setResult($)
  }
}
trait CallGraphTreeNode extends CallGraphNode     with GraphTreeNode /* TBD: `with Variables` to be moved to here*/
trait CallGraphLeafNode extends CallGraphTreeNode with GraphLeafNode

trait N_code_fragment[R] extends CallGraphLeafNode with ScriptResultHolder[R] with ExecutionResult {
  type T <: T_code_fragment[R,_]
  override def asynchronousAllowed: Boolean = true
  var msgAAToBeExecuted: CallGraphMessage = null
  var priority = 0 // < 0 is low, > 0 is high
  
  private[this] var _isExecuting = false
  override def isExecuting = _isExecuting
  def isExecuting_=(value: Boolean) = _isExecuting = value
  def mustPropagateResultValue = template.mustPropagateResultValue
}

object CallGraphNode {
  type Child  = CallGraphNode
  type Parent = CallGraphNode
  
  var currentStamp = 0; // used for searching common ancestors
  
  def nextStamp() = {currentStamp = currentStamp+1; currentStamp}
  
  // answer the stepsUp'th N_n_ary_op ancestor node
  def upInGraphToNAry(n: CallGraphTreeNode) : N_n_ary_op = {
    var a = n
    while (true) {
      a match {
        case nary: N_n_ary_op => return nary
        case _ =>
      }
      a = a.parent.asInstanceOf[CallGraphTreeNode]
    }
    return null // dummy exit
  }
  
  // find the lowest launch_anchor common ancestor of a node
  //
  def getLowestLaunchAnchorAncestor(n: CallGraphNode): N_launch_anchor = 
    n match {
      case nla@N_launch_anchor(_) => nla
      case _ => getLowestSingleCommonAncestor(n, _.isInstanceOf[N_launch_anchor] ).asInstanceOf[N_launch_anchor]
  }
      
      
  // find the lowest single common ancestor of a node, that fulfills a given condition:
  // easy when there is 0 or 1 parents
  //
  def getLowestSingleCommonAncestor(n: CallGraphNode, condition: (CallGraphNode)=>Boolean): CallGraphNode = {
    val lsca = n.lowestSingleCommonAncestor
    if (lsca==null) return null
    if (condition(lsca)) return lsca
    return getLowestSingleCommonAncestor(lsca,condition)
  }
  
  private def stampNodeWithAncestors(n: CallGraphNode): Unit = {
    if (n.stamp==currentStamp) {
      // this one has already been stamped this round, so here branches come together
      // maybe it is the oldest of such nodes thus far; then record it
      if (lowestSingleCommonAncestor==null
      ||  lowestSingleCommonAncestor.index > n.index)
      {
        lowestSingleCommonAncestor = n
      }
    }
    else
    {
      n.stamp = currentStamp;
      n.forEachParent(stampNodeWithAncestors)
    }
  }
  
  private var lowestSingleCommonAncestor: CallGraphNode = null
  
  // find the lowest common ancestor of a collection of nodes:
  // for each node, stamp upwards in the graph; 
  // each time when the current stamp is encountered, that node may be the lowest common ancestor
  // the oldest of such candidates is considered the one.
  //
  // NOTE: this will return a false LCA when the true LCA has multiple paths to the graph source!!!
  private def getLowestSingleCommonAncestor(nodes: List[CallGraphNode]): CallGraphNode = {
    nextStamp() 
    lowestSingleCommonAncestor = null
    nodes.foreach(stampNodeWithAncestors(_))
    return lowestSingleCommonAncestor
  }
}
