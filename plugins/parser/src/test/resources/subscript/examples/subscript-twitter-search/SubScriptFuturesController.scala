package subscript.twitter.app.controller
import subscript.file

import subscript.DSL._
import subscript.swing.Scripts._

import subscript.twitter.api._
import subscript.twitter.app.view.View

import subscript.twitter.util.{InterruptableFuture, CancelException}
import subscript.twitter.util.InterruptableFuture.Implicits.executionContext
import scala.util.{Success, Failure}

/**
 * Created by andre on 2/22/15.
 */
class SubScriptFuturesController(val view: View) extends Controller {
  import scala.language.implicitConversions

  def startDebug() = {
      val executor = new subscript.vm.executor.CommonScriptExecutor[Any]
      val debugger = new subscript.vm.SimpleScriptDebuggerClass
      executor.traceLevel = 2
      debugger.traceLevel = 4
      _execute(liveScript, debugger, executor)
  }
  def start() = _execute(liveScript)
  
  val fWait   = InterruptableFuture {Thread sleep keyTypeDelay}
  val fSearch = InterruptableFuture {twitter.search(view.searchField.text, tweetsCount)}
  val faulter = InterruptableFuture {throw new Exception("Never faulter")}

  implicit script f2s(intf: InterruptableFuture[_]): Any =
    @{intf.execute().onComplete {case aTry => there.executeForTry(aTry)}}: {.  .}

  script..

    liveScript = initialize ; (mainSequence / ..)...

    initialize = view.main(Array[String]())

    mainSequence = anyEvent(view.searchField)
                   fWait
                   fSearch ~~(ts:Seq[Tweet])~~> updateTweetsView(ts)
                         +~/~(t: Throwable )~~> setErrorMsg(t)

    updateTweetsView(ts: Seq[Tweet]) = @gui: {view.setTweets(ts)}
    setErrorMsg     (t : Throwable ) = @gui: {view.setError(t.toString)}
}
