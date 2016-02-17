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

  script..
    //live = handle_task ; spam / receive_confirmation ; sleep(timeout) fail / handle_success ; ...
    
    times(x: Int) = while(here.pass<x)

    live = handle_task
           (times(maxRetries) {target ! currentMessage} sleep(retryInterval)) fail
           / receive_confirmation ( handle_success || sleep(timeout) fail )
           ...
  
    spam = times(maxRetries) {target ! currentMessage} sleep(retryInterval)
    
    sleep(t: FiniteDuration) = {* sleepFd(t) *}
    
    fail = {taskRequester ! Failure(taskId); reset(None)}
    
    handle_task = <<t @ Task(data, id) => reset(Some(id)); currentMessage = t>>
    /*
    handle_task = r$({
      case t @ Task(data, id) =>
        reset(Some(id))
        currentMessage = t
    })
    */
    
    receive_confirmation = <<s @ ReceiptConfirmation(id) if id == taskId && sender == target>> 
    // receive_confirmation = r$({case s @ ReceiptConfirmation(id) if id == taskId && sender == target =>})
    
    handle_success = <<s @ Success(id, Some(data)) if id == taskId => taskRequester ! s; reset(None)>>
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
