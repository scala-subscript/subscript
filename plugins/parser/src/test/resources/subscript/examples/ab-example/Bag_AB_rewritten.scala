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

abstract class Bag_AB_Application extends SimpleSubscriptApplication {
  def getTitle: String
  
  val A = new Button("A") {enabled = false}
  val B = new Button("B") {enabled = false}
  val a = new Button("a") {enabled = false}
  val b = new Button("b") {enabled = false}
  
  val top          = new MainFrame {
    title          = getTitle
    location       = new Point    (800,0)
    preferredSize  = new Dimension(400,70)
    contents       = new BorderPanel {
      add(new FlowPanel(A, B, a, b), BorderPanel.Position.North) 
    }
  }
  override def live = _execute(liveScript)
def liveScript: subscript.vm.Script[Any]
}

object Bag_A extends Bag_AB_Application {
  def getTitle = "Bag: live = A (live&a)"

 override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => A), subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => liveScript), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)))} 
}

object Bag_AB extends Bag_AB_Application {
  def getTitle = "Bag: live = A (live&a) + B (live&b)"

 override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => A), subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => liveScript), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a))), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => B), subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => liveScript), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b))))}
}

object Bag_AB_loops extends Bag_AB_Application {
  def getTitle = "Bag = (A a & ..) & (B b & ..)"

 override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par(subscript.DSL._par(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => A), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a)), subscript.DSL._optionalBreak_loop), subscript.DSL._par(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => B), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => b)), subscript.DSL._optionalBreak_loop))}
}

