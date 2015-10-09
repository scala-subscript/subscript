package subscript.example
import subscript.file

import scala.swing._
import scala.swing.event._

import subscript.DSL._

import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._


// Subscript sample application: A..B
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala


abstract class ABApplication extends SimpleSubscriptApplication {
  import scala.language.implicitConversions
  
  def getTitle: String

  val A = new Button("A")           {enabled       = false}
  val B = new Button("B")           {enabled       = false}
  val X = new Button("Exit")        {enabled       = false}
  val ABLabel  = new Label("..A;B") {preferredSize = new Dimension(45,26)}
  val outputTA = new TextArea       {editable      = false}
  
  val top          = new MainFrame {
    title          = getTitle 
    location       = new Point    (0,0)
    preferredSize  = new Dimension(300,70)
    contents       = new BorderPanel {
      add(new FlowPanel(A, B, X, ABLabel), BorderPanel.Position.North) 
    }
  }
  
 override def  live = _execute(liveScript)
 script liveScript: Any
 script..
                   doExit = X + Key.Escape
           
 implicit script vkey(??k: Key.Value): Any = vkey2(top, ??k)
}

object AsBX extends ABApplication {
 def getTitle = "A..; B; exit"
 override script liveScript = A ..; B; doExit
}
object AsBsX extends ABApplication {
 def getTitle = "A..B; exit"
 override script liveScript = A .. B; doExit
}
