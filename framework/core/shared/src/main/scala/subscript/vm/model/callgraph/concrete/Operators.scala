// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._
import subscript.vm.model.template.TemplateNode

case class N_annotation[CN<:CallGraphNode.Child, CT<:TemplateNode.Child](template: T_annotation[CN,CT])
    extends CallGraphTreeNode
{
  type T = T_annotation[CN,CT]
  def there: CN = children.head.asInstanceOf[CN]
}

case class N_1_ary_op(template: T_1_ary_op) extends CallGraphTreeNode {
  type T = T_1_ary_op
  var continuation: Continuation1 = null
}

// Conditional Boolean operators
case class N_if(template: T_if)
  extends CallGraphTreeNode 
  {type T = T_if}

case class N_if_else(template: T_if_else) 
  extends CallGraphTreeNode 
  {type T = T_if_else}


// Conditional script operators
case class N_do_then(template: T_do_then) 
  extends CallGraphTreeNode 
  {type T = T_do_then}

case class N_do_else(template: T_do_else) 
  extends CallGraphTreeNode 
  {type T = T_do_else}

case class N_do_then_else(template: T_do_then_else) 
  extends CallGraphTreeNode 
  {type T = T_do_then_else}


/** N-ary 
 *  
 *  Exclusiveness
 * 
 * The ; and + operators are fully exclusive.
 * That means that if an atomic action in one operand happens, all other operands are excluded.
 * 
 * The “/“ operator is semi-exclusive: if an atomic action in one operand happens,
 * all other operands that are older (more to the left) are excluded; for the rest “/“ acts much like “|”.
 * 
 * “&&” and “||” exclude operands in special cases:
 * for && if one operand fails (i.e. it is deactivated without having a recent success);
 * for || when one operand is deactivated while having a recent success. 
 * 
 */
case class N_n_ary_op(template: T_n_ary_op, isLeftMerge: Boolean)
    extends CallGraphTreeNode
    with    OptionalChildrenState
    with    VariablesContainer
    
    with subscript.vm.executor.parts.Tracer
{
  type T = T_n_ary_op   
  
  // Local state
  var isIteration                = false
  var hadFullBreak               = false
  var activationMode             = ActivationMode.Active
  var continuation: Continuation = null
  var lastActivatedChild: Child  = null // may be a small memory leak

  
  // Helper and state methods
  def getLogicalKind = T_n_ary_op.getLogicalKind(template)
  def mustBreak = hadFullBreak = true
  

  // Overridden behavior
  override def addChild(c: Child) = {
    super.addChild(c); 
    if (isOptionalChild(c)) nActivatedOptionalChildren += 1
    lastActivatedChild = c
  }
  
  override def childChangesSuccess(child: Child) = {
val old_nActivatedChildrenWithSuccess = nActivatedChildrenWithSuccess
    val delta = if (child.hasSuccess) 1 else -1
    nActivatedChildrenWithSuccess += delta
    if (isOptionalChild(child)) nActivatedOptionalChildrenWithSuccess += delta
//println(f"$this%-14s child $child%-14s  changesSuccess to ${child.hasSuccess} nActivatedChildrenWithSuccess: $old_nActivatedChildrenWithSuccess => $nActivatedChildrenWithSuccess")
  }
  
  def traceLevel = 2
  def isOptionalChild(c:CallGraphNode.Child) = {
    
    val result =
   !aaHappenedInOptionalChildren      && 
    lastActivatedChild != null        &&
    indexChild_optionalBreak_last > 0 &&
    c.index > (if (indexChild_optionalBreak_last == lastActivatedChild.index) 
                   indexChild_optionalBreak_secondLast
              else indexChild_optionalBreak_last )   
              
    //traceAttributes(this, s"???? isOptionalChild c.index=${c.index} result=$result   ????")
     
    result
  }
  
  
  // Information
  override def infoString = extendedInfoString
  override def toString   = super.toString+(if(isIteration)" ..."else"")
}
