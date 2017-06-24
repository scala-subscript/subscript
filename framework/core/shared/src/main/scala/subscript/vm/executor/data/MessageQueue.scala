package subscript.vm.executor.data

import scala.collection.mutable.PriorityQueue
import subscript.vm._
import subscript.vm.executor._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import subscript.vm.model.callgraph._

class MessageQueue(val lock: AnyRef) extends MsgPublisher with MessagePriorities {self =>
  //private case class Enqueue(msg: CallGraphMessage) extends Operation {def commit = self enqueue msg}
  
  def isEmpty = collection.isEmpty
  
  /* Internal state */
  val ordering = new Ordering[CallGraphMessage] {
    def compare(x: CallGraphMessage, y: CallGraphMessage): Int = {
      var p = x.         priority - y.         priority; if (p != 0) return p 
          p = x.secondaryPriority - y.secondaryPriority; if (p != 0) return p
      return  x. tertiaryPriority - y. tertiaryPriority
    }
  }
  val collection = PriorityQueue[CallGraphMessage]()(ordering)
  
  private var _nMessages = 0
  private def nextMessageID = {_nMessages+=1; _nMessages}
  
  def mySynchronized[T0](s: String)(arg0: ⇒ T0): T0 = {
    //println("synchronized start: "+s)
    //try 
    synchronized{
      //try 
      arg0
      //finally println("synchronized code done: "+s)    
    }
    //finally println("synchronized end: "+s)
  }

  def insert(e: CallGraphMessage): Unit = {
    e.index = nextMessageID
    enqueue(e)
  }
 
  def enqueue(e: CallGraphMessage): Unit = {
    mySynchronized("enqueue: "+e) {collection += e; messageQueued(e)}
  }

  /*
   * For debugging only: trace that the given message is effectively removed.
   * The underlying priorityQueue does not support this.
   * For the time being the message should be implicitly be removed, e.g. by a "canceled" flag
   */
  def traceRemoval(m: CallGraphMessage) = {
    messageDequeued(m)
    //scriptGraphMessages -= m  is not allowed...FTTB we will ignore this message, by checking the canceled flag in the executor
  }
 
  def dequeue(minimalPriorityForAA: Int): CallGraphMessage = {
   mySynchronized("dequeue") {
    if (collection.isEmpty) return null
    
    if (minimalPriorityForAA > Int.MinValue) {
      val h = collection.head
      if (h.priority <= PRIORITY_CFToBeExecuted) {
        h match {
          case aatbe@CFToBeExecuted(n: N_code_fragment[_]) if (n.priority >= minimalPriorityForAA) =>
          case _ => return null
        }
      }
    }
    
    val result = collection.dequeue
    //if (callGraphMessageCount != callGraphMessages.length) {
    //  println("dequeue: "+callGraphMessageCount+" != " + callGraphMessages.length)
    //}
    messageDequeued(result)
    result
   }
  }
}

/**
 * Provides convenience methods to quickly insert certain messages.
 */
trait MQExtras {this: MessageQueue =>
  
  def doNeutral(n: CallGraphNode) =
    if (n.getLogicalKind_n_ary_op_ancestor!=LogicalKind.Or) {
      n.hasSuccess = true
      insert(SuccessMsg(n))
    }
  
  def insertDeactivation(n:CallGraphNode,c:CallGraphNode) = insert(Deactivation(n, c, false))
  
  def insertContinuation(message: CallGraphMessage, child: CallGraphNode = null): Unit = {
   mySynchronized("insertContinuation") { // absolutely needed; if absent race conditions may well harm execution
    val n = message.node.asInstanceOf[ N_n_ary_op]
    var c = n.continuation 
    
    // Continuations are merged with already existing ones
    // TBD: make separate priorities of continuations...
    // e.g. a continuation for CFActivated should not be merged (probably) with one for AAHappened
    if (c==null) {
      c = new Continuation(n)
    }
    if (c.childNode==null) // should be improved
    {
      c.childNode = 
        if (child!=null) child
        else message match {
          case Break(an,c,m) => c
          case SuccessMsg(an,c) => c
          case _ => message.node
        }
    }
    message match {
      case a@ Activation  (node: CallGraphNode) => c.activation = a
      case a@Deactivation (node: CallGraphNode,
                          child: CallGraphNode, excluded: Boolean) => c.deactivations ::= a
      case a@SuccessMsg   (node: CallGraphNode,
                          child: CallGraphNode)  => c.success = a
      case a@Break        (node: CallGraphNode, 
                          child: CallGraphNode, 
                 activationMode: ActivationMode.ActivationModeType)  => c.break = a
      case a@CFActivated  (node: CallGraphNode, 
                          child: CallGraphNode) =>  c.aaActivated = a
      case a@CAActivated  (node: CallGraphNode, 
                          child: CallGraphNode) =>  c.caActivated = a
      case a@AAHappened   (node: CallGraphNode, 
                          child: CallGraphNode, _) =>  c.aaHappeneds ::= a
    }
    if (n.continuation==null) {
       n.continuation = c
       insert(c)
    }
    else messageContinuation(message, c)
   }
  }
  // insert a continuation message for a unary operator
  def insertContinuation1(message: CallGraphMessage): Unit = {
   mySynchronized("insertContinuation1") {
    val n = message.node.asInstanceOf[N_1_ary_op]
    var c = n.continuation
    if (c==null) {
      c = new Continuation1(n)
      n.continuation = c
      enqueue(c)
    }
    enqueue(Continuation1(n))
   }
  }
}

trait TrackToBeExecuted extends MessageQueue {
  override def insert(m: CallGraphMessage) {
    super.insert(m)
    track(m, true)
  }
  override def traceRemoval(m: CallGraphMessage) {
    super.traceRemoval(m)
    track(m, false)
  }
  override def dequeue(minimalPriorityForAA: Int) = {
    val msg = super.dequeue(minimalPriorityForAA)
    if (msg ne null) track(msg, false)
    msg
  }
  
  private def track(m: CallGraphMessage, track: Boolean) {
    // AA nodes keep track of associated "to be executed" messages.
    // This way, if such a node is excluded by another process, it will be able to get such a message out of the queue;
    // then that message may be garbage collected and the link to the node will be gone, so that the node may also 
    // be garbage collected
    m match {
      case maa@CFToBeExecuted  (n: N_code_fragment[_]) => n.msgCFToBeExecuted = if (track) maa else null
      case maa@AAToBeReexecuted(n: N_code_fragment[_]) => n.msgCFToBeExecuted = if (track) maa else null
      case _ =>
    }
  }
}
