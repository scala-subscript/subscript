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

trait ContinuationHandler {this: ScriptExecutor[_] with Tracer =>
  import msgQueue._
  import graph.{activateFrom, linkNode}

  val continuationHandler: MessageHandler = {
    case a@Continuation(_) => handleContinuation(a)
  }

  /*
   * handleContinuation:
   *
   * The most complicated method of the Script Executor:
   * determine what an N-ary operator will do
   * after it has received a set of messages. Potential outcomes include:
   * - activate next operand and/or have success
   * - suspend, resume, exclude
   * - deactivate
   * - nothing
   *
   * The decision is based on three aspects:
   * - the kind of operator
   * - the state of the node
   * - the received messages
   * 
   * The main complication is due to the existence of the optional break operand (".").
   * For sequential composition this is relatively easy, as the optional break causes
   * the sequential operator to succeed itself, and also to activate the next operand.
   * For parallel operators and the semi-parallel disrupt operator ("/") the optional break
   * is really complicated (as it seems at least for now); 
   * more information follows below.
   * 
   * Note that the "loop with optional break" operand ".." is essentially a "." combined with a "...".
   * The effect of the latter is rather simple, as it makes the current n_ary_operator a loop, 
   * as if its template subtree is repeated infinitely many times 
   * (only with the pass counter being incremented)
   * 
   * Note that the disrupt operator ("/") behaves just like "|" except for exclusion:
   * - when an atomic action happens in an operand, all operands more to the left are excluded
   * - when an operand deactivates successfully then all operands more to the right are excluded
   * 
   * The rest of this comment is rather long, probably too long; 
   * FTTB it is left here until something better and more concise is available
   * 

On activation all n-ary operators activate their leftmost operands.
Moreover, a “continuation” message is inserted into the message queue,
so that, as soon as the just instigated activation of a subtree has ended,
the next operand may start activating, and so on, until no new activations are needed.

For a sequential operator (“;”) a next operand is activated when its predecessor
operand has success; if there is no such a next operand, then the sequential
operator itself succeeds. The latter also happens when an operand has an optional-break.

For the exclusive-or operator, the behaviour of “.” is different;
as only in 1 operand of this “+” operator an atomic action may happen,
the only sensible meaning for the optional break is: only activate the
next operand if just before an atomic action has been activated.
The meaning of “just before” needs clarification.
These are the operands activated since after the previous optional-break operand,
or from the start if there is no such previous optional-break operand.
If there were no such “just before” operands, as in . + x and in x + . + .
for the second optional-break, then keep on activating
(an alternative definition, not necessarily worse, would be: stop activation).

For parallel operators and for the disruption operator the behaviour is
more difficult, especially for those that are and-like. Note that for an
or-like n-ary operator have success, it is enough for any of its operands
to have a recent success. A success of an operand is recent if after it
has happened no atomic action it its subtree has happened.
An and-like parallel operator has in principle success when all its operands
have success, but the optional break complicates matters.

E.g. in x & break? & y, the optional break says that y is optional;
as long as no atomic action inside y has happened, the success
of & depends on the x operand.

Another effect of such an optional group of operands is that
after another “break?” activation only continues
as soon as the first atomic action in the optional operands has happened;

To define "more precisely" when “next activations” of parallel operators
happen, we consider their templates to be of the form

xi & break? & yi & break? & zi

Here xi etc stand for groups of operands that have no optional-breaks
activated; each group size is 0 or higher.
& stands here for any operator of: & && | || /

On activation xi and “break?” activate.
Activation continues with the next group (yi).

Then the second "break?" is activated. There are two possibilities:
- no atomic actions had been activated in yi.
  Then zi is activated, and maybe the next "break?" is activated etc.
- some atomic actions had been activated in yi.
  Then the yi are FTTB considered to be an "optional group", and activation stops.
  The optionality disappears as soon as an atomic action happens in yi;
  then also the activation resumes.

For the determination of successfulness of an and-parallel operator
the members an optional group of operands do not count.

So far for "break?".

Note that for parallel operators not only the successfulness of the current operands is relevant,
but also of the past operands.

E.g. a past operand of & that has deactivated without recent success
is a failure, and it prevents & from ever having success.
Likewise a past operand of | that has deactivated with a recent success makes sure that
| has a success after each atomic action happening somewhere in the descendant subtree of |.
These effects do not hold when the operand is in an optional group.

  * 
  */
  def handleContinuation(message: Continuation): Unit = new Decisions(message) {
    
    if (!node.isExcluded) {
    
      decideActivationProgress
      decideExclusion
      decideSuccess

    }
    trace

    executeDecisions // if isExcluded this may well do a Deactivation
  }

  trait ActivationProgressState {this: Stateful =>
    var activateNext              = false
    var activationEnded           = false
    var activationEndedOptionally = false

    var nextActivationTemplateIndex = 0
    var nextActivationPass = 0

    var shouldSucceed = false
  }

  trait ExclusionState {this: Stateful =>
    var nodesToBeExcluded : Seq[CallGraphNode.Child] = null
    var nodesToBeSuspended: Seq[CallGraphNode.Child] = null
  }

  trait Stateful extends ActivationProgressState with ExclusionState {
    val message: Continuation
    val node = message.node.asInstanceOf[N_n_ary_op]

    val isSequential = node.template.kind match {
      case ";" | "|;"  | "||;" |  "|;|" => true
      case _ => false
    }
  }

  trait ActivationProgressDecisions extends Stateful {
    private var activateNextOrEnded = false

    def decideActivationProgress {
      if (isSequential) {
        sequential 
      }
      else if (node.activationMode!=ActivationMode.Inactive)
      {
        node.template.kind match {
          
          case "+" | "|+" | "|+|"                   => plus
          
          case kind if T_n_ary_op.isLeftMerge(kind) => parallelLeftMerge // not really implemented yet

          // for parallel operators:
          // - if aaActivated && isOptionalChild: nActivatedOptionalChildren++
          // - if optionalBreak activated: set indexChild_optionalBreak_last and determine activateNextOrEnded:
          //           . & a     => no pause; a is optional
          //     (+) & . & a     => no pause; a is optional
          //      a  & . & b     => pause; after a happens b is activated as optional
          // - if aaHappened  && isOptionalChild: resetNActivatedOptionalChildren
          // - if success     && isOptionalChild: nActivatedOptionalChildrenWithSuccess++ "automatically" in State.hasSuccess_

          // Note: both message.activation and message.aaHappeneds may be != Nil
          // e.g. in a b / . after a happened b is activated, and only then the continuation at / is handled
          //
          case _ => if (message.activation != null) {
	                    val b = message.break
				        if (b==null) {
	                      traceAttributes(node, "common case for parallel operator: proceed activation")
				          activateNextOrEnded = true 
				        }
				        else if (b.activationMode==ActivationMode.Optional) {
				          if (node.aaActivated_notBeforeLastOptionalBreak) { // so now pause
	                        traceAttributes(node, "optional break encountered; pause activation")
	                        node.aaActivated_notBeforeLastOptionalBreak = false
				                  node.indexChild_optionalBreak_last = b.child.index
				          }
				          else
				          {
	                  traceAttributes(node, "optional break encountered; proceed activation anyway since no atomic actions had been activated")
				            node.indexChild_optionalBreak_last = b.child.index
				            activateNextOrEnded = true
				          }
				        }
				        else {
				           traceAttributes(node, "mandatory break encountered; activation stopped")
				        }
                    } 
          
                    if (message.aaHappeneds!=Nil && 
                        // check that node has been paused, 
                        //  and is waiting for an AA to happen in a child after optionalBreak_secondLast (which may be -1)
                        node.indexChild_optionalBreak_last == node.lastActivatedChild.index && // the "pausing" test; maybe not explicit enough
                        message.aaHappeneds.exists{a => a.child.index > node.indexChild_optionalBreak_secondLast})
                    {
                          traceAttributes(node, "AA Happened; optional children become mandatory; proceed activation")
                          activateNextOrEnded = true
                          node.activationMode = ActivationMode.Active
                          node.aaHappened_resetNActivatedOptionalChildren
                    }
                    
                    if (activateNextOrEnded) clarifyActivationProgress(node.lastActivatedChild)
        }
      }
    }

    private def sequential {
      val s = message.success
      val b = message.break
      var nodeAfterWhichToActivate: CallGraphNode = null
      
      if (b!=null) {
        activateNextOrEnded = true
        nodeAfterWhichToActivate = b.child
        if (b.activationMode==ActivationMode.Optional)
          activationEndedOptionally = true
        else node.mustBreak
      }

      if (s!=null) {
        activateNextOrEnded = true
        nodeAfterWhichToActivate = s.child
      }
      if (activateNextOrEnded) clarifyActivationProgress(nodeAfterWhichToActivate)
    }

    private def clarifyActivationProgress(nodeAfterWhichToActivate: CallGraphNode)  {
      nextActivationTemplateIndex = nodeAfterWhichToActivate.template.indexAsChild+1
      nextActivationPass = nodeAfterWhichToActivate.pass

      message.node.activationMode = ActivationMode.Active

      // def ackNext = !(activationEnded || activationEndedOptionally)
      if (node.hadFullBreak) activationEnded = true
      else if (nextActivationTemplateIndex==message.node.template.children.size) {
        if (message.node.isIteration) {
          nextActivationTemplateIndex = 0
          nextActivationPass += 1
          activateNext = true
        }
        else activationEnded = true
      }
      else activateNext = true
    }

    /**
     * "+" node activations may be broken by "." if no atomic actions had been activated
     */
    private def plus {
      val a = message.aaActivated
      val c = message.caActivated
      val b = message.break

      activateNextOrEnded = if (b==null) true
                       else if (b.activationMode==ActivationMode.Optional) a!=null || c!=null
                       else false

      if (activateNextOrEnded) clarifyActivationProgress(node.lastActivatedChild)
    }

    private def parallelLeftMerge {
       val aa = message.aaActivated
       val ca = message.caActivated
       val as = message.aaHappeneds
       val b  = message.break
       activateNextOrEnded = aa==null && ca==null ||
                             as!=Nil  && as.exists( (as:AAHappened) => as.node==node.lastActivatedChild )
       if (b!=null) {
         // ???
       }
    }
  }

  trait ExclusionDecisions extends Stateful {

    // exclusions may be needed when
    // - atomic actions happen
    // - operands are deactivated
    
    def decideExclusion = node.template.kind match {
      case "/"  | "|/"                  => disrupt
      case "|/|"                        => disrupt; parallel // these 2 won't bite; one for aaHappened; other for deactivation
      case "&&" | "&&:" | "||"  | "||:" => parallel
      case ";"  | "|;"  | "+"   | "|+"  => sequenceOrChoice
      case "||;"| "||+"                 => sequenceOrChoice; parallel // won't bite one another either
      case _ if T_n_ary_op.isSuspending(node.template) => suspensions
      case _ =>
    }
           
    private def suspensions {
      val aaHappenedChildren = message.aaHappeneds.map(_.child)
      if (!aaHappenedChildren.isEmpty) {
        val s = aaHappenedChildren.head // there will be only 1
        if (s.aaHappenedCount==1) { // only done when an atomic action happens for the first time here
            node.template.kind match {
              case "%&" | "%;"   => nodesToBeSuspended = node.children diff List(s)
              case "%"           => nodesToBeExcluded  = node.children.filter(_.index < s.index) 
                                    nodesToBeSuspended = node.children.filter(_.index > s.index)
              case "%/" | "%/%/" => nodesToBeSuspended = node.children.filter(_.index < s.index) 
            }
          }
      }
    }
    
    private def sequenceOrChoice {
      val aaHappenedChildren = message.aaHappeneds.map(_.child)
      if (!aaHappenedChildren.isEmpty) {
        val indexes_aaHappenedChildren = aaHappenedChildren.map(_.index)
        nodesToBeExcluded = node.children.filter(c => !indexes_aaHappenedChildren.contains(c.index))
      }
    }
        
    private def disrupt {
      nodesToBeExcluded = Nil
      // deactivate to the left when an atomic action has happened somewhere in an operand
      val aaHappenedChildren = message.aaHappeneds.map(_.child)
      if (!aaHappenedChildren.isEmpty) {
        val indexes_aaHappenedChildren = aaHappenedChildren.map(_.index)
        val maxIndex = indexes_aaHappenedChildren.max
        nodesToBeExcluded ++= node.children.filter{n => n.index<maxIndex}
      // Note: for |/ and |/| such a happening atomic action may be shared among multiple operands;
      // Then deactivate to the left from the rightmost of such operands.
      // However, the next outcommented code line would not catch that correctly, since it would also allow for multiple AAHappeneds that
      // resulted from asynchronous execution, rather than from script sharing (communication):
        
      // nodesToBeExcluded ++= node.children.filter{n => val index = n.index; index<maxIndex && !indexes_aaHappenedChildren.contains(index)}

        //println(s">>>>>>>>> disrupt maxIndex=$maxIndex nodesToBeExcluded=$nodesToBeExcluded)      
      }
      
      // deactivate to the right when one has finished successfully
      // Note: more than 1 operand may do so; 
      // then deactivate more to the right from the leftmost of these already deactivated operands
      val deactivatedChildren = message.deactivations.filter(!_.excluded).map(_.child)
      val indexes_deactivatedChildrenHavingSuccess = deactivatedChildren.filter(_.hasSuccess).map(_.index)
      
      if (!indexes_deactivatedChildrenHavingSuccess.isEmpty) {
        val minIndexDeactivedChildHavingSuccess = indexes_deactivatedChildrenHavingSuccess.min
        nodesToBeExcluded ++= node.children.filter(c => c.index>minIndexDeactivedChildHavingSuccess && !indexes_deactivatedChildrenHavingSuccess.contains(c.index))
        activateNext = false // override possibly earlier decision; needed for a/./b after a happened.
        activationEnded = true // possibly message.node.activationMode should be set to Inactive 
      }
    }

    private def parallel {
      val isLogicalOr = T_n_ary_op.getLogicalKind(node.template.kind)==LogicalKind.Or
      /*
       * Result decisive nodes are the nodes that decide
       * the result of the whole operator.
       * For example, one single node that ended in success decides
       * the result of an Or-parallel operator, and one single node that
       * ended with failure decides the result of an And-parallel operator.
       */
      val deactivatedChildren = message.deactivations.map(_.child)
      val resultDecisiveNodes = deactivatedChildren.filter(_.hasSuccess==isLogicalOr)

      //deactivatedChildren.foreach{n => println(s"deactivatedChild: $n hasSuccess=${n.hasSuccess}")}
      
      if (!resultDecisiveNodes.isEmpty) {
        nodesToBeExcluded = node.children //diff deactivatedChildren
        activateNext = false // override possibly earlier decision; needed for a/./b after a happened.
        activationEnded = true // possibly message.node.activationMode should be set to Inactive 

        //println(s"nodesToBeExcluded=$nodesToBeExcluded")        
      }
    }
  }

  trait SuccessDecisions extends Stateful {

    def decideSuccess = if (!shouldSucceed && !node.hasSuccess)
      node.template.kind match {
        case ";" => sequential
        case "/" => disrupt

        case _   => T_n_ary_op.getLogicalKind(node.template.kind) match {
          case LogicalKind.None =>
          case LogicalKind.And  => logicalAnd
          case LogicalKind.Or   => logicalOr
        }
      }

    private def sequential = shouldSucceed = activationEnded || activationEndedOptionally

    private def disrupt    = shouldSucceed = message.success != null ||
                                     message.aaHappeneds.exists(_.child.index<node.rightmostChildThatEndedInSuccess_index) ||
                                     node.nActivatedChildrenWithSuccess > 0

    private def logicalAnd  = shouldSucceed = !activateNext &&
                      node.nActivatedMandatoryChildrenWithoutSuccess == 0

    private def logicalOr   = shouldSucceed = node.nActivatedChildrenWithSuccess > 0
  }

  trait DecisionsExecution extends Stateful {
    def executeDecisions {
      succeed
      exclude
      activate
    }

    private def succeed = if (shouldSucceed) {node.hasSuccess=true; insert(SuccessMsg(node))}   // TBD: prevent multiple successes at same "time"

    private def exclude = {
      if (nodesToBeExcluded !=null) nodesToBeExcluded .foreach(n => insert(Exclude(node, n)))
      if (nodesToBeSuspended!=null) nodesToBeSuspended.foreach(n => insert(Suspend(      n)))
    }

    private def activate {
      // this line is wrong...
      // sequential operators CAN have more than 1 activated child
      // in case we deal with an optional break: (. a; b)
      // if (isSequential) activateNext = activateNext && node.children.isEmpty
      if (activateNext) {
        val t = message.node.template.children(nextActivationTemplateIndex)
        activateFrom(message.node, t, Some(nextActivationPass))
        val activation = if (message.activation != null) message.activation else Activation(message.node)

        val nary_op_isLeftMerge = node match {
          case N_n_ary_op (t: T_n_ary, isLeftMerge) => isLeftMerge
          case _                                    => false
        }
        if (!nary_op_isLeftMerge) insertContinuation(activation, node)
      }
      else if (node.children.isEmpty) insertDeactivation(node, null)
    }
  }


  class Decisions(val message: Continuation) extends
      ActivationProgressDecisions  with
      ExclusionDecisions with
      SuccessDecisions   with
      DecisionsExecution {
    node.continuation = null

    def trace {
      traceAttributes(node, "Finally")
      traceAttribute("activateNext", activateNext)
      traceAttribute("activationEnded", activationEnded)
      traceAttribute("activationEndedOptionally", activationEndedOptionally)
      traceAttribute("shouldSucceed", shouldSucceed)
    }
  }
}
