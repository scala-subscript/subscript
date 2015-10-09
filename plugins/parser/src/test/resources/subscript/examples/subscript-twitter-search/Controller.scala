package subscript.twitter.app.controller

import subscript.twitter.api.Twitter
import subscript.twitter.app.view.View

/**
 * Created by anatolii on 11/29/14.
 */
trait Controller {

  val view: View

  val twitter      = Twitter()
  val tweetsCount  = 10
  val keyTypeDelay = 500

  def start(): Unit

}
