package subscript.akka

import subscript.language
import subscript.Predef._
import subscript.objectalgebra.Trigger

import subscript.DSL._
import scala.collection.mutable.ListBuffer
import subscript.vm._
import subscript.vm.executor._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import akka.actor._
import subscript.vm.model.callgraph._


trait SubScriptActorPlus extends Actor {

  val runner    = SSARunnerV1Scheduler
  val terminate = new Trigger

  // Ability to handle a message is defined by a sender and a handler partial function. Message is communicated to the parent dataflow via trigger.
  // `Either` allows to check via a predefined ActorRef or accept an arbitrary ActorRef as a parameter, so that it will be set to a `sender` later.
  private val handlers = ListBuffer[(Either[ActorRef, ActualOutputParameter[ActorRef]], Trigger, PartialFunction[Any, Script[Any]])]()

  // Scripts
  script..
    live: Any
    die       = {!if (context ne null) context stop self!}
    lifecycle = [live || terminate] ; die

    registerHandler(act: Either[ActorRef, ActualOutputParameter[ActorRef]]) =
      val vt         = new Trigger  // The value sent from `act` will pop from `vt` and will be returned from this script to be handled by the parent dataflow
      val handlerFun = here.ancestor(9).getProperty[String, PartialFunction[Any, Script[Any]]]("then").get  // PartialFunction from the dataflow this `act` is LHS of. Will handle the matching messages. TBD: ancestor(n) should be replaced by a more descriptive methods, no guessing on the argument `n` should be done.
      val handler    = (act, vt, handlerFun)  // Everything together

      do handlers += handler  // Save the actor, the trigger and the handler function of the dataflow
      [vt ~~(msg)~~> [do handlers -= handler; ^msg]]^  // Wait for a suitable message to pop from `vt`, then deregister the handler and return the message to be handled normally by the dataflow.

  implicit script..
    actorRef2script(a: ActorRef)        = registerHandler(Left ( a))
    actorRefParam2script(??a: ActorRef) = registerHandler(Right(?a))

  
  // Callbacks
  override def aroundPreStart() {
    Debug.info(s"$this aroundPreStart INIT")  
    runner.launch([lifecycle])
    super.aroundPreStart()
    Debug.info(s"$this aroundPreStart EXIT")
  }

  override def aroundReceive(receive: Actor.Receive, msg: Any) {
    synchronized {
      sendSynchronizationMessage(this)
      wait()
    }

    // Check the incoming message for appropriate sender and ability to be handled by one of the handlers
    // Previously was in handlers.synchronized. If something goes wrong, wrap it back in it.
    handlers.collectFirst {
      case (Left (actorRef     ), trigger, handler) if actorRef           == sender  && handler.isDefinedAt(msg) => trigger.trigger(msg)
      case (Right(actorRefParam), trigger, handler) if actorRefParam.matches(sender) && handler.isDefinedAt(msg) => actorRefParam.transferFunction(sender); trigger.trigger(msg)
    } match { // Was the message handled?
      case None    => super.aroundReceive( receive        , msg); Debug.info(s"$this aroundReceive did NOT handle msg   sender: $sender msg: $msg")
      case Some(_) => super.aroundReceive({case _: Any =>}, msg); Debug.info(s"$this aroundReceive handled  sender: $sender msg: $msg")      
    }
  }

  override def aroundPostStop() {
    terminate.trigger
    super.aroundPostStop()
  }

  def receive: Actor.Receive = {case _ =>}

  def sendSynchronizationMessage(lock: AnyRef) {
    val vm = runner.executor
    vm insert SynchronizationMessage(vm.rootNode, lock)
  }
}

case class SynchronizationMessage(node: CallGraphNode, lock: AnyRef) extends CallGraphMessageN {
  type N = CallGraphNode
  override def priority = PRIORITY_InvokeFromET - 1  // After all actors are launched
}

// was: import scala.actors.Logger
object Debug extends Logger("") {}
class Logger(tag: String) {
  private var lev = 2

  def level = lev
  def level_= (lev: Int) = { this.lev = lev }

  private val tagString = if (tag == "") "" else " ["+tag+"]"

  def info     (s: String)  = if (lev > 2) System.out.println(   "Info" + tagString + ": " + s)
  def warning  (s: String)  = if (lev > 1) System.err.println("Warning" + tagString + ": " + s)
  def error    (s: String)  = if (lev > 0) System.err.println(  "Error" + tagString + ": " + s)
  def doInfo   (b: => Unit) = if (lev > 2) b
  def doWarning(b: => Unit) = if (lev > 1) b
  def doError  (b: => Unit) = if (lev > 0) b
}