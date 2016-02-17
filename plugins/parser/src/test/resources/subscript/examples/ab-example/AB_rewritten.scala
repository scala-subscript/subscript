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
def liveScript: subscript.vm.Script[Any]
def doExit = subscript.DSL._script[Any](None, Symbol("doExit")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => X), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => Key.Escape))}
           
 implicit def vkey(k: subscript.vm.FormalConstrainedParameter[Key.Value]): subscript.vm.Script[Any] = subscript.DSL._script[Any](None, Symbol("vkey"), k.~??(Symbol("k"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => vkey2(subscript.DSL._maybeVarCall("top"), subscript.DSL._maybeVarCall("subscript.vm.ActualAdaptingParameter(k)")))}
}

object AsBX extends ABApplication {
 def getTitle = "A..; B; exit"
 override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => A), subscript.DSL._optionalBreak_loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => B), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => doExit))}
}
object AsBsX extends ABApplication {
 def getTitle = "A..B; exit"
 override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => A), subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => B)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => doExit))}
}
