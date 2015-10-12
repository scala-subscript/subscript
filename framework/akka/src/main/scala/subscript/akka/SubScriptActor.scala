package subscript.akka
import subscript.language

import subscript.DSL._
import scala.collection.mutable.ListBuffer
import subscript.vm._
import subscript.vm.executor._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import akka.actor._
import subscript.vm.model.callgraph._

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
trait SubScriptActor extends Actor {

  val runner: SubScriptActorRunner = SSARunnerV1Scheduler

  private object Terminator { // TBD: find better name; something with Blocker
    var executor: EventHandlingCodeFragmentExecutor[Any] = null

    script block = @{executor = new EventHandlingCodeFragmentExecutor(there, there.scriptExecutor)}: {. .}
    def release = executor.executeMatching(isMatching=true)
  }

  private val callHandlers = ListBuffer[PartialFunction[Any, Unit]]()



  // Scripts
  script live: Any
  script terminate = Terminator.block
  script die       = {!if (context ne null) context stop self!}

  script r$(handler: PartialFunction[Any, ScriptNode[Any]])
  = var s:ScriptNode[Any]=null
    @{var handlerWithExecuteAA = handler andThen {hr => {s = hr; there.eventHappened}}
                          synchronized {callHandlers += handlerWithExecuteAA}
      there.onDeactivate {synchronized {callHandlers -= handlerWithExecuteAA}}
    }: {. Debug.info(s"$this.r$$") .}
    if s != null then s



  script lifecycle = [live || terminate] ; die
  
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

    callHandlers.synchronized { // TBD: why is synchronized needed
      callHandlers.collectFirst { case handler if handler isDefinedAt msg => handler(msg) } match {
        case None    => super.aroundReceive( receive        , msg); Debug.info(s"$this aroundReceive did NOT handle msg   sender: $sender msg: $msg")
        case Some(_) => super.aroundReceive({case _: Any =>}, msg); Debug.info(s"$this aroundReceive handled  sender: $sender msg: $msg")
      }
    }
  }

  override def aroundPostStop() {
    Terminator.release
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
