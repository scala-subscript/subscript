package subscript.example.taskprocessor.ssactors
import subscript.file

import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import scala.concurrent.duration._
import scala.language.postfixOps

import subscript.Predef._
import subscript.akka._

import subscript.example.taskprocessor.Protocol._
import subscript.example.taskprocessor.Protocol


/*
 * Di = Data integral
 * Df = Data forked
 * Ri = Result integral
 * Rf = Result forked
 */
class FrontProcessor[Di, Df, Ri, Rf](
    val fork: (Di, Int) => Seq[Df],
    val join: Seq[Rf]   => Ri
  ) extends SubScriptActor {  
  var processors   : Map[Protocol.Processor, Protocol.Proxy] = Map()
  var responses    : List[Rf]                                = Nil
  var taskId       : Long                                    = -1
  var taskRequester: ActorRef                                = null
      
  script..
    live = (handle_task ; while (!ready) (handle_success + handle_failure) ; deliverResult) + handle_configuration
           ...
  
    deliverResult = {
      val result: Ri = join(responses)               // Compile final result from pieces via `join` function
      taskRequester ! Success(taskId, Some(result))  // Report success
      reset(None)
    }
  
    
    handle_task = <<
      Task(data: Di, id) =>
        reset(Some(id))
        val forked: Seq[Df] = fork(data, processors.size)
        propagate(forked)
    >>
    /*
    handle_task = r$({
      case Task(data: Di, id) =>
        reset(Some(id))                                    // Prepare internal state for processing
        val forked: Seq[Df] = fork(data, processors.size)  // Split (fork) data to pieces
        propagate(forked)
    })
    */
    
    handle_success = <<
      Success(id, Some(data: Rf)) if id == taskId && processors.exists {case (_, p) => p == sender} =>
        responses :+= data
    >>
    /*
    handle_success = r$({
      case Success(id, Some(data: Rf)) if id == taskId && processors.exists {case (_, p) => p == sender} =>
        responses :+= data  // Save piece of data
    })
    */
    
    handle_failure = <<
      f @ Failure(id) if id == taskId && processors.exists {case (_, p) => p == sender} =>
        taskRequester ! f
        reset(None)
    >>
    /*
    handle_failure = r$({
      case f @ Failure(id) if id == taskId && processors.exists {case (_, p) => p == sender} =>
        taskRequester ! f
        reset(None)
    })
    */
    
    handle_configuration = <<
      Configuration(processors) =>
        reset(None)
        updateProcessors(processors)
        sender ! Ready
    >>
    /*
    handle_configuration = r$({
      case Configuration(processors) =>
        reset(None)                   // If currently busy - become idle
        updateProcessors(processors)  // Kill old processors, create new processors on need
        sender ! Ready                // Report readiness
    })
    */
  
  
  /**
   * State transmission management.
   */
  def reset(maybeTaskId: Option[Long]) {
    // Clear previous state
    responses     = Nil
    taskId        = -1
    taskRequester = null
    
    // Set new state if needed
    maybeTaskId.foreach {id =>
      taskId        = id
      taskRequester = sender
    }
  }

  /**
   * Processor registration machinery.
   */
  def updateProcessors(nProcessors: Seq[ActorRef]) {
    val currentProcessors = processors.toList
    
    // Analyze
    val unchanged = currentProcessors filter {case (processor, _) => nProcessors contains processor}
    val toKill    = currentProcessors diff unchanged
    val toCreate  = nProcessors diff (currentProcessors map {case (p, _) => p})
    
    // Process
    for ((_, proxy) <- toKill) context stop proxy
    
    // Save
    processors = unchanged.toMap ++ toCreate.map {p => (p, proxy(p))}.toMap
  }
  
  /**
   * Each piece of data goes to one processor.
   */
  def propagate(data: Seq[Df]) =
    for {((_, p), d) <- processors zip data} p ! Task(d, taskId)
  
  /**
   * If it's true, we are ready to compute final result from pieces
   */
  def ready = processors.size == responses.size
  
  /**
   * Describes how to create proxy. Proxy is needed to provide
   * additional delivery guarantees.
   */
  def proxy(p: Protocol.Processor): Protocol.Proxy = {
    val props = Props(classOf[Proxy], p, maxRetries, retryInterval, timeout)
    context actorOf props
  }
    
    
   /**
   * This method exists for debugging purposes
   */
  def status {
    val s = s"""
      | Task      : $taskId
      | Task req  : $taskRequester
      | Processors: $processors
      | Responses : $responses
    """.stripMargin
    println(s)
  }
}
