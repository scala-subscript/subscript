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
      
def live = subscript.DSL._script[Any](None, Symbol("live")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handle_task), subscript.DSL._seq(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("!ready")
}), subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handle_success), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handle_failure))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => deliverResult)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handle_configuration)), subscript.DSL._loop)}
def deliverResult = subscript.DSL._script[Any](None, Symbol("deliverResult")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._normal[Any] (_node => {
  implicit val here = _node

      val result: Ri = subscript.DSL._maybeVarCall("join(subscript.DSL._maybeVarCall(\"responses\"))")               // Compile final result from pieces via `join` function
      subscript.DSL._maybeVarCall("taskRequester ! Success(subscript.DSL._maybeVarCall(\"taskId\"), subscript.DSL._maybeVarCall(\"Some(subscript.DSL._maybeVarCall(\\\"result\\\"))\"))")  // Report success
      subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"None\"))")

}, true)}
def handle_task = subscript.DSL._script[Any](None, Symbol("handle_task")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case Task(data: Di, id) =>
        subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"Some(subscript.DSL._maybeVarCall(\\\"id\\\"))\"))")
        val forked: Seq[Df] = subscript.DSL._maybeVarCall("fork(subscript.DSL._maybeVarCall(\"data\"), subscript.DSL._maybeVarCall(\"processors.size\"))")
        subscript.DSL._maybeVarCall("propagate(subscript.DSL._maybeVarCall(\"forked\"))")

null
}))}
def handle_success = subscript.DSL._script[Any](None, Symbol("handle_success")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case Success(id, Some(data: Rf)) if subscript.DSL._maybeVarCall("id == taskId && processors.exists {case (_, p) => subscript.DSL._maybeVarCall(\"p == sender\")}") =>
        subscript.DSL._maybeVarCall("responses :+= data")

null
}))}
def handle_failure = subscript.DSL._script[Any](None, Symbol("handle_failure")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case f @ Failure(id) if subscript.DSL._maybeVarCall("id == taskId && processors.exists {case (_, p) => subscript.DSL._maybeVarCall(\"p == sender\")}") =>
        subscript.DSL._maybeVarCall("taskRequester ! f")
        subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"None\"))")

null
}))}
def handle_configuration = subscript.DSL._script[Any](None, Symbol("handle_configuration")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => r$({
case Configuration(processors) =>
        subscript.DSL._maybeVarCall("reset(subscript.DSL._maybeVarCall(\"None\"))")
        subscript.DSL._maybeVarCall("updateProcessors(subscript.DSL._maybeVarCall(\"processors\"))")
        subscript.DSL._maybeVarCall("sender ! Ready")

null
}))}
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
