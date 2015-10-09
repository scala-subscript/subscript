package subscript.twitter.app.controller

import subscript.twitter.app.view.View
import subscript.twitter.util.{InterruptableFuture, CancelException}

import subscript.twitter.util.InterruptableFuture.Implicits.executionContext
import scala.util.{Success, Failure}

import scala.swing.{Swing, Reactor}

/**
 * Created by anatolii on 11/29/14.
 */
class PureController(val view: View) extends Controller with Reactor {

  def start() = {initialize; bindInputCallback}

  def initialize = view.main(Array())

  def bindInputCallback = {
    listenTo(view.searchField.keys)

    val fWait   = InterruptableFuture {Thread sleep keyTypeDelay}
    val fSearch = InterruptableFuture {twitter.search(view.searchField.text, tweetsCount)}

    // React to the keys only if the delay between presses is >= keyTypeDelay (0.5 seconds)
    // Without this feature, every key typed will deplete the Twitter API calls limit.
    reactions += {case _ =>
    //fWait.execute().flatMap {case _ => fSearch.execute()}.onSuccess {case tweets => Swing.onEDT {view.setTweets(tweets)}}
      fWait.execute().flatMap {
        case _                          => fSearch.execute()}.onComplete {
        case Success(tweets)            => Swing.onEDT {view.setTweets(tweets)}
        case Failure(e:CancelException) => Swing.onEDT {view.setStatus("...")}
        case Failure(ex)                => Swing.onEDT {view.setError (ex.toString)}}
    }
  }

}

