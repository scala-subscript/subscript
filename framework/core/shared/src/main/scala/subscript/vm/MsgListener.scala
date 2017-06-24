package subscript.vm

import scala.collection.mutable.ListBuffer

trait MsgListener {
  def messageHandled     (m: CallGraphMessage) = {}
  def messageQueued      (m: CallGraphMessage) = {}
  def messageDequeued    (m: CallGraphMessage) = {}
  def messageContinuation(m: CallGraphMessage, c: Continuation) = {}
  def messageAwaiting  = {}
  
  def attach(publisher: MsgPublisher) {publisher addListener this}
}

trait MsgPublisher extends MsgListener {
  private val listeners = ListBuffer[MsgListener]()
  def addListener   (l: MsgListener) = listeners += l
  def removeListener(l: MsgListener) = listeners -= l
  
  override def messageHandled     (m: CallGraphMessage) = listeners.foreach {_.messageHandled(m) }
  override def messageQueued      (m: CallGraphMessage) = listeners.foreach {_.messageQueued(m)  }
  override def messageDequeued    (m: CallGraphMessage) = listeners.foreach {_.messageDequeued(m)}
  override def messageContinuation(m: CallGraphMessage, c: Continuation) = listeners.foreach {_.messageContinuation(m, c)}
  override def messageAwaiting = listeners.foreach {_.messageAwaiting}
}
