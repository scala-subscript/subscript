package subscript.example.taskprocessor.ssactors
import subscript.file

import akka.actor._
import subscript.example.taskprocessor.Protocol._

import subscript.Predef._
import subscript.akka._

/**
 * Receives tasks. Executes some job on data. Responds with success.
 * Maybe fails (on JVM fatal failure or ordinary exception) and stays silent.
 */
class Processor[Df, Rf](processor: Df => Rf) extends SubScriptActor {
  
  var taskId       : Long             = -1
  var taskRequester: ActorRef         = null
  var worker       : Option[ActorRef] = None
  
  
  script..
    live = task success ...
    
    task = <<Task(data: Df, id) => sender ! ReceiptConfirmation(id); reset(Some(id)); process(data)>>
    /*
    task = r$({
      case Task(data: Df, id) =>
        sender ! ReceiptConfirmation(id)
        reset(Some(id))
        process(data)
    })
    */
    
    success = <<s @ Success(id, Some(data: Rf)) if id == taskId && worker.exists(_ == sender) =>
      taskRequester ! s
      reset(None)
    >>
    /*
    success = r$({
      case s @ Success(id, Some(data: Rf)) if id == taskId && worker.exists(_ == sender) =>
        taskRequester ! s
        reset(None)
    })
    */
  
  
  /**
   * This method controls state transmissions between `busy` and `idle`.
   */
  def reset(maybeId: Option[Long]) {
    // Set the internal state to its primeval condition
    taskId        = -1
    taskRequester = null
    worker.foreach(context stop _)  // Kill worker if it exists and processes (irrelevant) task
    
    // If there's a new task - set the state appropriately
    maybeId.foreach {id =>
      taskId        = id
      taskRequester = sender
    }
  }
  
  /**
   * Spawns a worker and gives it a task to process.
   */
  def process(data: Df) {
    val task: () => Rf = () => processor(data)
    worker = Some(context actorOf Props(classOf[Worker[Rf]], task, taskId))
  }
  
}

/**
 * Does some actual job and responds with success.
 */
class Worker[R](task: () => R, id: Long) extends Actor {
  context.parent ! Success[R](id, Some(task()))
  def receive = {case x => context.parent forward x}
}