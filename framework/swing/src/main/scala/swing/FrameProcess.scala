package subscript.swing

import subscript.language

import scala.swing.event._

import subscript.Predef._
import subscript.objectalgebra._
import subscript.swing.Scripts._


trait FrameProcess extends scala.swing.Frame with SSProcess {

  val closed = new Trigger

  override def closeOperation() {
    closed.trigger
  }

  implicit script vkey(??k: Key.Value) = vkey2: this, ??k

  override script lifecycle =
    @{visible=true; there.onDeactivate(close())}: super.lifecycle^ / closed

}
