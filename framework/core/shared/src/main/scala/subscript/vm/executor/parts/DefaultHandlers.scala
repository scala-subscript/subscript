package subscript.vm.executor.parts

import subscript.vm._
import subscript.vm.executor._
import subscript.vm.executor.data._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import subscript.DSL._
import scala.collection.mutable.Buffer
import subscript.vm.model.callgraph._

import CodeExecutor._
import CallGraph._
import MessageHandlers._

/** To DATA section; don't change */
trait DefaultHandlers extends ContinuationHandler {this: ScriptExecutor[_] with Tracer =>
  import msgQueue._
  import graph.{activateFrom, linkNode}
  
  /*
   * Handle an activation message.
   * This involves:
   * 
   * execute activation code, if defined
   * execute specific code, e.g. for if, while, script call
   * insert an activation message to create a child node
   */
  def handleActivation(message: Activation): Unit = {
      executeCodeIfDefined(message.node, message.node.onActivate)
      message.node match {
           //case n@N_root            (t: T_1_ary     ) => activateFrom(n, t.child0)
           case n@N_code_tiny                  (t) => n.hasSuccess = true; executeCode(n); if (n.hasSuccess) doNeutral(n); insertDeactivation(n,null)
           case n@N_localvar                   (t) => if (t.isLoop) setIteration_n_ary_op_ancestor(n);
            n.n_ary_op_ancestor.initLocalVariable(t.localVariable.name, n.pass, executeCode(n));doNeutral(n);insertDeactivation(n,null)
           case n@N_privatevar                 (t) => n.n_ary_op_ancestor.initLocalVariable(t.name, n.pass, n.getLocalVariableHolder(t.name).value)
           case n@N_code_normal                (_) => insert(CFActivated(n,null)); insert(CFToBeExecuted(n))
           case n@N_code_unsure                (_) => insert(CFActivated(n,null)); insert(CFToBeExecuted(n))
           case n@N_code_threaded              (_) => insert(CFActivated(n,null)); insert(CFToBeExecuted(n))

           case n@( N_code_eventhandling      (_) 
                  | N_code_eventhandling_loop (_)) => insert(CFActivated(n,null)) 
                                                       // ehNodesAwaitingExecution.append(n) not used; could be handy for debugging
              
           case n@N_break                      (t) =>                                    doNeutral(n); insert(Break(n, null, ActivationMode.Inactive)); insertDeactivation(n,null)
           case n@N_optional_break             (t) =>                                    doNeutral(n); insert(Break(n, null, ActivationMode.Optional)); insertDeactivation(n,null)
           case n@N_optional_break_loop        (t) => setIteration_n_ary_op_ancestor(n); doNeutral(n); insert(Break(n, null, ActivationMode.Optional)); insertDeactivation(n,null)
           case n@N_loop                       (t) => setIteration_n_ary_op_ancestor(n); doNeutral(n); insertDeactivation(n,null)
           case n@N_delta                      (t) =>                     insertDeactivation(n,null)
           case n@N_epsilon                    (t) => insert(SuccessMsg(n)); insertDeactivation(n,null)
           case n@N_nu                         (t) => doNeutral(n);          insertDeactivation(n,null)
           case n@N_while                      (t) => setIteration_n_ary_op_ancestor(n); 
                                                      if (executeCode(n)) doNeutral(n)
                                                      else               {doNeutral(n); insert(Break(n, null, ActivationMode.Inactive))}
                                                      insertDeactivation(n,null)
                                                                       
           case n@N_launch                     (t) => activateFrom(CallGraphNode.getLowestLaunchAnchorAncestor(n), t.child0, Some(0)); doNeutral(n); insertDeactivation(n,null)
           case n@N_launch_anchor              (t) => activateFrom(n, t.child0, Some(0))
           case n@N_1_ary_op                   (t) => activateFrom(n, t.child0); insertContinuation1(message)
           case n@N_annotation                 (t) => activateFrom(n, t.child0); executeCode(n)
           case n@N_if                         (t) => if (executeCode(n)) activateFrom(n, t.child0) else {doNeutral(n); insertDeactivation(n,null)}
           case n@N_if_else                    (t) => if (executeCode(n)) activateFrom(n, t.child0) 
                                                                    else  activateFrom(n, t.child1)
           case n@N_do_then                    (t) => activateFrom(n, t.child0)
           case n@N_do_else                    (t) => activateFrom(n, t.child0)
           case n@N_do_then_else               (t) => activateFrom(n, t.child0)
           case n@N_n_ary_op                   (t, isLeftMerge) => val cn = activateFrom(n, t.children.head); if (!isLeftMerge) insertContinuation(message, cn)
           case n@N_call                       (t) => val s: Script[_] = executeCode(n)
                                                      if (n.t_callee!=null) linkNode(n, s, s, None)
                                                      else {
                                                        insert(CAActivated   (n,null))
                                                        insert(CAActivatedTBD(n))
                                                      }
           case n@Script                       (t, _*) => activateFrom(n, t.child0)   // ???????????
      }      
  }
  
  def propagateResult(child: CallGraphNode, node:CallGraphNode, forSuccess: Boolean) = {
    //println(s"propagateResult child: $child node: $node hasSuccess: ${node.hasSuccess} forSuccess: $forSuccess")
    
    def propagate(): Unit = node.asInstanceOf[ScriptResultHolder[Any]].resultPropagationDestination[Any] match {
      case sn: Script[Any] =>
        def allChildren(n: TreeNode): Seq[TreeNode] = n.children.flatMap {c => allChildren(c) :+ c}
        val children = allChildren(sn.template).filter {c => c.isInstanceOf[T_code_fragment[_, _]] || c.isInstanceOf[T_call[_]]}
        // println(s"Child count for $sn: ${children.size}. Children: $children")
        
        // If the script has only one child, propagate result. Otherwise, let the carrets decide the result.
        if (children.size == 1 && children.contains(node.template)) node.asInstanceOf[ScriptResultHolder[Any]].propagateResult

      case _ => node.asInstanceOf[ScriptResultHolder[Any]].propagateResult
    }

    (child,node) match {
           case (_,    ncf:N_code_tiny    [_]) => if (ncf.mustPropagateResultValue && forSuccess != ncf.failed    ) propagate()
           case (_,    ncf:N_code_fragment[_]) => if (ncf.mustPropagateResultValue && forSuccess == ncf.hasSuccess) propagate()
           case (scr:Script[_], cal:N_call[_]) => cal.setResult(scr.$)
                                                  if (cal.mustPropagateResultValue && forSuccess == scr.hasSuccess) {
																			               //println(s"src: $scr")
																			               //println(s"node: ${node}")
																			               //println(s"node.scriptNode: ${node.scriptNode}")
																			               
																			               propagate()
																			             }
           case _ =>
    } 
  }
  
  /*
   * Handle a success message
   * 
   * for n_ary and 1_ary nodes if the message comes from a child node:
   *   postpone further processing by inserting a continuation message 
   *   
   * for a script call node: transfer parameters
   * 
   * set the node's hadSuccess flag
   * execute "onSuccess" code, if defined
   * insert success messages for each parent node
   */
  def handleSuccess(message: SuccessMsg): Unit = {
         // The following lines had been inserted on 10 April 2014 [c8f7a57], and outcommented on 22 April 2014.
         // These were wrong: a success message for a [while] would not be processed any more
         //if (message.node.hasSuccess) {
         //  return // should not occur?
         //}
         if (message.node.isExcluded) return // may occur in {. .}/a
    
         message.node match {
                                           // Note: message.child!=null is needed because of doNeutral(n) in handleDeactivation()
               case n@  N_do_then      (t  ) => if (message.child!=null && message.child.template==t.child0) {activateFrom(n, t.child1); return}
               case n@  N_do_then_else (t  ) => if (                       message.child.template==t.child0) {activateFrom(n, t.child1); return}
               case n@  N_do_else      (t  ) => if (                       message.child.template==t.child0) {if (n.getLogicalKind_n_ary_op_ancestor==LogicalKind.Or) return}
               case n@  N_annotation   (_  ) => {} // onSuccess?
                                           // Note:            message.child.template==t.child0) {if (n.getLogicalKind_n_ary_op_ancestor==LogicalKind.Or) return}
               case n@  N_1_ary_op     (t  ) => if (message.child         != null) {insertContinuation1(message); return}
               case n@  N_n_ary_op     (_,_) => if (message.child         != null) {insertContinuation (message); return}
               case n@  N_launch_anchor(_  ) => if(n.nActivatedChildrenWithoutSuccess > 0) {return}
               case n@  N_call           (_) => if (!n.allActualParametersMatch) {return}
                                                n.transferParameters
               case _ =>
          }
         message.node.hasSuccess = true
         propagateResult(message.child, message.node, forSuccess=true) // note: N_call may do a return, a few lines higher

         executeCodeIfDefined(message.node, message.node.onSuccess)
         message.node.forEachParent(p => insert(SuccessMsg(p, message.node)))
  }
  
  /*
   * Handle a deactivation message. 
   * If the receiving node is a n_ary operator and there is a sending child node,
   * then postpone further processing by inserting a continuation message.
   * TBD: add a case for a 1_ary operator
   * 
   * Insert deactivation messages for all parent nodes
   * Execute code for deactivation, if defined
   * Unlink the node from the call graph
   */
  def handleDeactivation(message: Deactivation): Unit = {
       val node = message.node

       node match {
           case n@N_n_ary_op (_: T_n_ary, _) => if(!message.excluded
                                                &&  message.child!=null) {
                                                  if (message.child.hasSuccess) {
                                                     n.childThatEndedInSuccess_index(message.child.index)
                                                  }
                                                  else {
                                                     n.aChildEndedInFailure = true
                                                  }
                                                  insertContinuation(message); 
                                                  return}
           case n@N_launch_anchor(_) => if (!n.children.isEmpty) return
           case _ =>
       }
       
       // this should possibly not be here:
       if(message.child!=null) { 
         node.hasSuccess = message.child.hasSuccess
       }
       else if(message.excluded) { 
         node.hasSuccess = false // code fragments
       }
       if(!message.excluded) propagateResult(message.child, node, forSuccess=false)
       
       node match {
           case n@N_do_then     (t)  => if (!message.excluded && message.child!=null
                                                              && message.child.template==t.child0 && !message.child.hasSuccess) {doNeutral(n); 
                                                                                                                                insertDeactivation(n,null); return} else if(!n.children.isEmpty) return
           case n@N_do_else     (t)  => if (!n.isExcluded && message.child.template==t.child0 && !message.child.hasSuccess) {activateFrom(n, t.child1); return} else if(!n.children.isEmpty) return
           case n@N_do_then_else(t)  => if (!n.isExcluded && message.child.template==t.child0 && !message.child.hasSuccess) {activateFrom(n, t.child2); return} else if(!n.children.isEmpty) return
           case _ => 
      }
      node.forEachParent(p => insertDeactivation(p,node))
      executeCodeIfDefined(node, node.onDeactivate)
      if (!message.excluded 
      &&  !node.hasSuccess) executeCodeIfDefined(node, node.onFailure)
      disconnect(childNode = node)
  }

  /*
   * Handle an CFActivated message: activated atomic actions 
   * for n_ary and 1_ary nodes if the message comes from a child node:
   *   postpone further processing by inserting a continuation message 
   *   
   * insert CFActivated messages for each parent node
   */
  def handleCFActivated(message: CFActivated): Unit = {
          message.node match {
               case n@  N_1_ary_op      (t: T_1_ary        )  => if(message.child!=null) {
                                                                   insertContinuation1(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case n@  N_n_ary_op      (_: T_n_ary, _     )  => if(message.child!=null) {
                                                                   n.aaActivated_notBeforeLastOptionalBreak = true
                                                                   insertContinuation(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case _ => 
          }
          message.node.forEachParent(p => insert(CFActivated(p, message.node)))
  }
/*
  /*
   * Handle an CAActivated message: activated communications 
   * This may be of interest for a "+" operator higher up in the graph: 
   *   it may have to proceed with activating a next operand, 
   *   in case it had been "paused" by a optional break operand (. or ..)
   *
   * for n_ary and 1_ary nodes if the message comes from a child node:
   *   postpone further processing by inserting a continuation message 
   *   
   * insert CAActivated messages for each parent node
   */
  def handleCAActivated(message: CAActivated): Unit = {
          message.node match {
               case n@  N_1_ary_op      (t: T_1_ary        )  => if(message.child!=null) {
                                                                   insertContinuation1(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case n@  N_n_ary_op      (_: T_n_ary, _     )  => if(message.child!=null) {
                                                                   insertContinuation(message)
                                                                   //don't return; just put the continuations in place
                                                                 }
               case _ => 
          }
          message.node.forEachParent(p => insert(CAActivated(p, message.node)))
  }
  
  /*
   * Communication handling features: still in the think&try phase
   * Not ready for testing
   */
  def handleCAActivatedTBD(message: CAActivatedTBD): Unit = {
    if (CommunicationMatchingMessage.activatedCommunicatorCalls.isEmpty) {
      insert(CommunicationMatchingMessage)
    }
    CommunicationMatchingMessage.activatedCommunicatorCalls += message.node
  }
  // TBD: process all fresh CA nodes to activate prospective communications
 def handleCommunicationMatchingMessage = {
    var startingCommunicationsOpenForMorePartners: List[N_communication] = Nil
    for (acc <- CommunicationMatchingMessage.activatedCommunicatorCalls) {
      // check out the associated communication relations:
      // if one of these may be activated with help of pending CA partners, then do so
      // else make this CA call pending as well
      
      var i = 0
      while (i < acc.communicator.roles.length && acc.children.isEmpty) {
        val cr = acc.communicator.roles(i)
        i += 1
        tryCommunication(acc, cr)
      }
      if (acc.children.isEmpty) {
        acc.communicator.instances += acc // TBD: remove again when acc is deactivated, using stopPending
      }
    }
    CommunicationMatchingMessage.activatedCommunicatorCalls.clear()
  }
        
  // a communication may become active when
  // all mandatory positions for partners at the same instance may be filled
  // partners should have compatible parameters, be active in parallel, and obey network topology
  def tryCommunication(freshCall: N_call, freshCallRole: CommunicatorRole): Boolean = {
    val communication = freshCallRole.communication
    
    def tryCommunicationWithPartners(partners: List[N_call]): Boolean = {
        def canCommunicateWithPartners(n: N_call): Boolean = {
          var i = 0
          while (i < partners.length) {
            // TBD: check for parallel locations
            val p = partners(i)
            val r = communication.communicatorRoles(i)
            // TBD: check parameter compatibilities
            
            // TBD: check network topology
          }
          return true
        }
      var i = partners.length // TBD: make it a List[List[N_call]]
        if (i == communication.communicatorRoles.length) {  
           val nc = N_communication(communication.template)
           // set nc.partners vv and make it activate
           nc.communication = communication
           nc.parents ++: partners
           for (p<-partners) {p addChild nc}
           //executeCode_call(nc);
           //activateFrom(nc, n.t_callee)}
           return true
        }
        else {
        val role = communication.communicatorRoles(i)
        val nodes = if (role==freshCallRole) List(freshCall)
                  else  role.communicator.instances
          var j = 0
          while (j < nodes.length) {
          val node = role.communicator.instances(j)
            j += 1
            if (canCommunicateWithPartners(node)) {
              if (tryCommunicationWithPartners(node::partners)) {
                return true;
              }
            }
          }
        return false
      }
    }
    // TBD: first try comms that may still grow (having multipicities other than One)
    return tryCommunicationWithPartners(Nil)
  }
*/          

  /*
   * Handle an AAHappened message
   *
   * Resets the node's hadSuccess flag
   * Increments the busyActions count
   * 
   * If the node is an n_ary or 1_ary operator: insert a continuation message 
   * If the node is a suspending operator: decide on what children to suspend
   * If the node is an exclusive opeator: decide on what children to exclude
   * 
   * Insert the exclude messages and suspend messages
   * For each parent node insert an AAHappened message
   * 
   * TBD: ensure that an n-ary node gets only 1 AAHappened msg 
   * after an AA started in a communication reachable from multiple child nodes (*)
   */
  def handleAAHappened(message: AAHappened): Unit = {
    if (message.child!=null) message.node.hasSuccess = false // if null then node is a code fragment
    message.node.numberOfBusyActions += (message.mode match {
      case     AtomicCodeFragmentExecuted =>  0
      case DurationalCodeFragmentStarted  =>  1
      case DurationalCodeFragmentEnded    => -1
    })
    message.node match {
       case n@N_1_ary_op(t: T_1_ary)    => insertContinuation1(message) //don't return; just put the continuations in place
       case n@N_n_ary_op(t: T_n_ary, _) => insertContinuation (message) //don't return; just put the continuations in place, mainly used for left merge operators
       case _ =>      
    }
    
    // message.child may be null now
    message.node.forEachParent(p => insert(AAHappened(p, message.node, message.mode)))
  }
  
  /*
   * Handle a break message (break  .   ..)
   * 
   * if the node is an n_ary operator:
   *   if the node is not already inactive, set its activation mode to the one specified by the break message
   *   insert a continuation message
   * else insert break messages for each parent node
   */
  def handleBreak(message: Break): Unit = {
      message.node match {
        case nn: N_n_ary_op =>
          if (nn.activationMode!=ActivationMode.Inactive) {
              nn.activationMode = message.activationMode
          }
          insertContinuation(message)
        case _ => message.node.forEachParent(p => insert(Break(p, message.node, message.activationMode)))
      }
  }
  
  /*
   * Handle an exclude message
   * 
   * Set the node's isExcluded flag
   * Interrupt asynchronously running code for the node, if any
   * 
   * If the node is a communication partner: make it stop pending (TBD)
   * If the node is an atomic action: remove CFToBeExecuted message, if any 
   *         (TBD: also remove AAToBeReExecuted message, if any?)
   *         inset a deactivation message
   * insert exclude messages for each child node
   */
  def handleExclude(message: Exclude): Unit = { // TBD: remove messages for the node; interrupt execution
    val n = message.node
    if (n.isExcluded) return // may occur for c in (a&b)/d, where a,b = c
    n.isExcluded = true
    
    if (message.node.template.isInstanceOf[TemplateCodeHolder[_,_]] && message.node.codeExecutor != null)
      message.node.codeExecutor.interruptAA
    
    n match {
      case cc: N_call[_] => cc.stopPending
      case aa: N_code_fragment[_] =>
        aa.codeExecutor.cancelAA
        if (aa.msgCFToBeExecuted != null) {
          traceRemoval(aa.msgCFToBeExecuted) // does not really remove from the queue; will have to check the canceled flag of the codeExecutor...
          aa.msgCFToBeExecuted = null
        }
        // TBD: also for caNodes!!
        insert(Deactivation(aa, null, excluded=true))
      case _ =>
    }
    executeCodeIfDefined(n, n.onExclude)
    
    n.children.foreach {c => insert(Exclude(n,c))}
  }
  
  /*
   * Handle a continuation message for an unary node
   */
  def handleContinuation1(message: Continuation1): Unit = {
    val n = message.node.asInstanceOf[N_1_ary_op]
    n.continuation = null
    // TBD
  }
  
  /*
   * Handle an CFToBeExecuted message
   * 
   * perform the codeExecutor's executeAA method
   * 
   * Note: the message may have been canceled instead of removed from the queue (was easier to implement),
   * so for the time being check the canceled flag
   */
  def handleCFToBeExecuted[T<:TemplateCodeHolder[R,_],R](message: CFToBeExecuted[R]) {
    val e = message.node.codeExecutor
    if (!e.canceled)  // temporary fix, since the message queue does not yet allow for removals
         e.executeAA
  }
  /*
   * Handle an AAToBeReexecuted message
   * 
   * insert an CFToBeExecuted message
   * 
   * Note: the message may have been canceled instead of removed from the queue (was easier to implement),
   * so for the time being check the canceled flag
   */
  def handleAAToBeReexecuted[T<:TemplateCodeHolder[R,_],R](message: AAToBeReexecuted[R]) {
    val e = message.node.codeExecutor
    if (!e.canceled) // temporary fix, since the message queue does not yet allow for removals
       insert(CFToBeExecuted(message.node)) // this way, failed {??} code ends up at the back of the queue
  }
  /*
   * Handle an CFExecutionFinished message
   * 
   * perform the codeExecutor's afterExecuteAA method,
   * which may insert success and deactivation messages in turn
   * 
   * Note:
   * A node's code executor has just finished execution. This may have been done asynchronously.
   * It has inserted an CFExecutionFinished, so that this will be handled synchronously in the main script executor loop.
   *
   */
  def handleCFExecutionFinished[T<:TemplateCodeHolder[R,_],R](message: CFExecutionFinished) {
     message.node.codeExecutor.afterExecuteAA
  }
  
  val defaultHandler: MessageHandler = {
      case a@ Activation        (_) => handleActivation   (a)
      case a@Continuation1      (_) => handleContinuation1(a)
      case a@Deactivation  (_,_, _) => handleDeactivation (a)
      case a@Suspend            (_) => {}
      case a@Resume             (_) => {}
      case a@Exclude          (_,_) => handleExclude    (a)
      case a@SuccessMsg       (_,_) => handleSuccess    (a)
      case a@Break        (_, _, _) => handleBreak      (a)
      case a@CFActivated      (_,_) => handleCFActivated(a)
   // case a@CAActivated      (_,_) => handleCAActivated(a)
   // case a@CAActivatedTBD     (_) => handleCAActivatedTBD(a)
      case a@AAHappened     (_,_,_) => handleAAHappened (a)
      case a@CFExecutionFinished(_) => handleCFExecutionFinished(a)
      case a@AAToBeReexecuted   (_) => handleAAToBeReexecuted   (a)
      case a@CFToBeExecuted     (_) => handleCFToBeExecuted     (a)
   // case CommunicationMatchingMessage => handleCommunicationMatchingMessage
    }
  
  val communicationHandler: MessageHandler = {
    case InvokeFromET(_, payload) => payload()
  }
}
