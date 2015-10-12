package subscript.akka
import subscript.language

import subscript.DSL._
import scala.collection.mutable.ListBuffer
import subscript.vm._
import subscript.vm.executor._
import akka.actor._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import scala.language.postfixOps


trait SubScriptActorRunner {

  def launch(s: ScriptNode[_])

  def execute(debugger: MsgListener)

  def system: ActorSystem

  def executor: CommonScriptExecutor[_]

  def doScriptSteps

}

object SSARunnerV1Scheduler extends SubScriptActorRunner {

  // Don't do heavy operations until needed
  lazy val system = ActorSystem()
  lazy val executor = ScriptExecutorFactory.createScriptExecutor[Any](true)

  var launch_anchor: N_launch_anchor = null

  def scheduledTaskDelay = 1 milliseconds

  def launch(s: ScriptNode[_]) = executor.invokeFromET {launch_anchor.launch(s)}

  script..
     // make a local anchor for launched actor processes,
     // so that we will be able to kill those here using the || and / operators
     live = @{launch_anchor=there.asInstanceOf[N_launch_anchor]}: [** {. .} **]

  def execute(debugger: MsgListener) {
    if (debugger!=null) debugger.attach(executor)
    executor.addHandler(synchMsgHandler)
    executor.initializeExecution(live)
    doScriptSteps_loop
//    executor run _live()
  }

  def doScriptSteps_loop: Unit = {
    doScriptSteps
    if (executor.hasActiveProcesses) {
      try system.scheduler.scheduleOnce(scheduledTaskDelay)(doScriptSteps_loop)
      catch {case _: IllegalStateException =>}  // Ignore certain things...
    }
  }

  def doScriptSteps = {
    executor.updateCollections()
    var handledMessage = executor.tryHandleMessage(minimalPriorityForAA = Int.MinValue)
    while (handledMessage!=null) {
      executor.updateCollections()
      handledMessage = executor.tryHandleMessage(minimalPriorityForAA = Int.MinValue)
    }
    executor.messageAwaiting
  }

  val synchMsgHandler: PartialFunction[CallGraphMessage, Unit] = {
    case SynchronizationMessage(_, lock) => lock.synchronized(lock.notify())
  }

}
