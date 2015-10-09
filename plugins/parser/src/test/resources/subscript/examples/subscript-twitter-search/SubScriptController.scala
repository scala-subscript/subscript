package subscript.twitter.app.controller
import subscript.file

import subscript.DSL._
import subscript.swing.Scripts._

import subscript.twitter.api._
import subscript.twitter.app.view.View

/**
 * Created by anatolii on 11/28/14.
 */
class SubScriptController(val view: View) extends Controller {

  def start() = {
      val executor = new subscript.vm.executor.CommonScriptExecutor[Any]
      val debugger = new subscript.vm.SimpleScriptDebuggerClass
      executor.traceLevel = 2
      debugger.traceLevel = 4
      _execute(liveScript)
      //_execute(_live(), debugger, executor)
  }
  //def start() = _execute(_live())

  def sleep(t: Long) = Thread sleep t

  script..

  //live = initialize ; mainSequence (-) / ..    works as well

    liveScript = initialize ; (mainSequence / ..)...

    initialize = view.main(Array())

    mainSequence = anyEvent(view.searchField)
                   waitForDelay
                   searchTweets ~~(ts:Seq[Tweet])~~> updateTweetsView(ts)
                              +~/~(t: Throwable )~~> setErrorMsg(t)

    waitForDelay = {* view.setStatus("waiting"  ); sleep(keyTypeDelay) *}
    searchTweets = {* view.setStatus("searching"); twitter.search(view.searchField.text, tweetsCount)*}

    updateTweetsView(ts: Seq[Tweet]) = @gui: {view.setTweets(ts)}
    setErrorMsg     (t : Throwable ) = @gui: {view.setError(t.toString)}
}
