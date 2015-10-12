package subscript.swing

import java.util.concurrent.Executors

import scala.util.{Success, Failure}
import scala.collection.mutable

import scala.swing._
import scala.swing.event._

import scala.concurrent._
import scala.concurrent.duration._

object PureScalaDebugger extends PureScalaDebuggerApp

class PureScalaDebuggerApp extends SimpleSwingApplication with GraphicalDebugger {
  implicit val executionContext = ExecutionContext fromExecutorService Executors.newCachedThreadPool()

  def live: Unit = {
    def again: Unit = Future {awaitMessageBeingHandled(true)}.flatMap {_ =>
      if (shouldStep) {
        Swing.onEDTWait(updateDisplay)
        Future firstCompletedOf Seq(stepCommand, autoStepCommand)
      } else Future.successful(())
    }.onComplete {
      case Success(_) =>
        messageBeingHandled(false)
        again
      case Failure(e) => e.printStackTrace()
    }

    again

    while (true) {
      waitForExit
      if (confirmExit) return
    }
  }

  // The reactorBank variable is needed to bypass a peculiarity of Scala Swing, which,
  // unfortunately, contains a bug.
  //
  // Publishers store their event listeners in the form of reaction functions.
  // Therefore, if you call listenTo(publisher) from a reactor, the reference
  // to this reactor will not be stored within the publisher. If there are no
  // other references to the reactor, it will be a subject for garbage collection.
  //
  // Publishers store the references to the reaction functions wrapped into a
  // java.lang.ref.WeakReference, which means that they will be garbage collected
  // if no other references to them exist.
  //
  // Given that typically the only other reference to a reaction function is
  // within a Reactor, it will be garbage collected together with the reactor.
  //
  // There are two solutions to this problem. The more elegant one is to add
  // override val reactions = new Reactions.Impl with Reactions.StronglyReferenced
  // to the anonymous reactors. This will make all the publishers to store
  // the reactions object as a strong reference rather then a weak one.
  // This approach doesn't work, however, due to the bug in Scala Swing, which doesn't
  // allow the reactors to unsubscribe from publishers if their reactions are StronglyReferenced.
  //
  // Another solution is to prevent the reactors themselves from the garbage collection
  // by storing them as a variable or in a collection.
  private[this] val reactorBank = mutable.ArrayBuffer[Reactor]()
  def futureButtonClick(button: Button): Future[Unit] = {
    val promise = Promise[Unit]()
    reactorBank += new Reactor {
      button.enabled = true
      listenTo(button)
      reactions += {
        case _: ButtonClicked =>
          promise.success(())
          button.enabled = false

          // Finalization logic
          deafTo(button)
          reactorBank -= this
      }
    }
    promise.future
  }

  def stepCommand = futureButtonClick(stepButton)
  def exitCommand = futureButtonClick(exitButton)

  def autoStepCommand: Future[Unit] = {
    lazy val never = Promise[Unit]().future
    if (autoCheckBox.selected) Future {waitForStepTimeout} else never
  }

  def waitForExit = Await.ready(exitCommand, Duration.Inf)

  override def main(args: Array[String]) = super.main(args)

}
