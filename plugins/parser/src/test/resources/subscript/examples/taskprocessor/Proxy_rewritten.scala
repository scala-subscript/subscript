package subscript.example.taskprocessor.ssactors
import subscript.file

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import subscript.example.taskprocessor.Protocol
import Protocol._
import subscript.akka._
import subscript.Predef._

/**
 * The job of this actor is to provide improved guarantees on message
 * delivery.
 * It will make sure that there were at least maxRetries attempts to deliver message,
 * then it will wait for successful response for `timeout`. If Success is received,
 * it will be redirected to parent, otherwise Failure will be sent to parent.
 */
class Proxy(
    target       : ActorRef,
    maxRetries   : Int,
    retryInterval: FiniteDuration,
    timeout      : FiniteDuration
) extends SubScriptActor {
  
  var taskId        : Long     = -1
  var taskRequester : ActorRef = null
  var currentMessage: AnyRef   = null
  
  def sleepFd(t: FiniteDuration) = Thread sleep t.toMillis

def times(x: Int) = subscript.DSL._script[Any](None, Symbol("times")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.pass<x")
})}
def live = subscript.DSL._script[Any](None, Symbol("live")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handle_task), subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => times(subscript.DSL._maybeVarCall("maxRetries"))), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("target ! currentMessage")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => sleep(subscript.DSL._maybeVarCall("retryInterval")))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => fail)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => receive_confirmation), subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handle_success), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => sleep(subscript.DSL._maybeVarCall("timeout"))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => fail))))), subscript.DSL._loop)}
def spam = subscript.DSL._script[Any](None, Symbol("spam")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => times(subscript.DSL._maybeVarCall("maxRetries"))), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("target ! currentMessage")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => sleep(subscript.DSL._maybeVarCall("retryInterval"))))}
def sleep(t: FiniteDuration) = subscript.DSL._script[Any](None, Symbol("sleep")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("sleepFd(subscript.DSL._maybeVarCall(\"t\"))")
}, true)}
def fail = subscript.DSL._script[Any](None, Symbol("fail")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("taskRequester ! Failure(subscript.DSL._maybeVarCall(\"taskId\"))"); subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"None\"))")
}, true)}
def handle_task = subscript.DSL._script[Any](None, Symbol("handle_task")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case t @ Task(data, id) => subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"Some(subscript.DSL._maybeVarCall(\\\"id\\\"))\"))"); subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"currentMessage\", subscript.DSL._maybeVarCall(\"t\"))")
null
}))}
def receive_confirmation = subscript.DSL._script[Any](None, Symbol("receive_confirmation")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case s @ ReceiptConfirmation(id) if subscript.DSL._maybeVarCall("id == taskId && sender == target") =>
null
}))}
def handle_success = subscript.DSL._script[Any](None, Symbol("handle_success")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case s @ Success(id, Some(data)) if subscript.DSL._maybeVarCall("id == taskId") => subscript.DSL._maybeVarCall("taskRequester ! s"); subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"None\"))")
null
}))}
    /*
    handle_success = r$({
      case s @ Success(id, Some(data)) if id == taskId =>
        taskRequester ! s
        reset(None)
    })
    */
  /**
   * In its first half, this method resets the state of the actor to
   * its primeval condition - stop all processing.
   * If the argument passed is not None, we start new processing (at least
   * reflect that it started in the internal state).
   */
  def reset(tid: Option[Long]) {
    // Stop all processing
    taskId         = -1
    taskRequester  = null
    currentMessage = null
    
    // Start new processing (at least document it)
    tid.foreach {id =>
      taskId = id
      taskRequester = sender
    }
  }
  
}
