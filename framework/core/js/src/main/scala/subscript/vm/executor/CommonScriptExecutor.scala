package subscript.vm.executor

import scala.util.{Try}
import subscript.vm._
import subscript.vm.executor.data._
import subscript.vm.executor.parts._
import subscript.DSL._
import scala.collection.mutable.Buffer
import subscript.vm.executor.data.MessageHandlers.MessageHandler
import subscript.vm.model.callgraph.ScriptResultHolder


class CommonScriptExecutor[S] extends AbstractScriptExecutor[S] with Tracer with
    DefaultHandlers {
  var traceLevel = 0

  // the next statement is outcommented because the locking interferes with the synchronisation of the queue itself
  // the purpose was to notify this executor when an asynchronously executed code fragment had finished
  // so that this executor could wake up in case it had been waiting.
  // this notification is now only done in doCodeThatInsertsMsgs_synchronized
  //msgQueue addListener new MessageQueuedNotifier(this)
  msgHandlers sInsert defaultHandler
  msgHandlers sInsert communicationHandler
  msgHandlers sInsert continuationHandler
  
  def fail = () // needed because this is a ResultHolder; probably a different place for _fail would be better
  
  def run[R<:S](s: Script[R]) = {
    initializeExecution(s)
    messageLoop()
    trace(s"$this Exit main loop. hasActiveProcesses = $hasActiveProcesses")
    $ = s.$
    this
  }

  def messageLoop(): Unit = while (hasActiveProcesses) {
    updateCollections()
    if (tryHandleMessage(Int.MinValue)==null) { awaitMessages; return } 
  }
  
  def awaitMessages {
    trace(s"$this awaitMessages")
    messageAwaiting
    if (msgQueue.collection.size == 0) { // looks stupid, but event may have happened&notify() may have been called during tracing
      trace(s"$this wait - start")
      startWaitingForMessages()
    }
    // note: there may also be deadlock because of unmatching communications
    // so there should preferably be a check for the existence of waiting event handling actions
  }

  var waiting = false

  def startWaitingForMessages(): Unit = {
    waiting = true
  }

  def endWaitingForMessages(): Unit = {
    waiting = false
    trace(s"$this wait - end")
    messageLoop()
  }
  
  /*
   * Execute the given code that may insert some call graph messages into the message queue.
   * This must be done in a synchronized way, and it may need to cause the call to wait()
   * in awaitMessages to end. Therefore this method does notify as well.
   */
  def doCodeThatInsertsMsgs_synchronized(code: =>Unit): Unit = { code; trace(s"$this notify"); if (waiting) endWaitingForMessages }
}
