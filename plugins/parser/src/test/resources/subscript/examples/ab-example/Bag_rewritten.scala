package subscript.example
import subscript.file

import scala.swing._
import scala.swing.event._

import subscript.DSL._

import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._

// Subscript sample application: a parallel recursive implementation of a Bag
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala
//

object Bag extends BagApplication

class BagApplication extends SimpleSubscriptApplication {
  
  val lA = new Label("A") {preferredSize = new Dimension(26,26)}
  val lB = new Label("B") {preferredSize = new Dimension(26,26)}
  val pA = new Button("+")          {enabled       = false}
  val pB = new Button("+")          {enabled       = false}
  val mA = new Button("-")          {enabled       = false}
  val mB = new Button("-")          {enabled       = false}
  val cA = new Label("")  {preferredSize = new Dimension(45,26)}
  val cB = new Label("")  {preferredSize = new Dimension(45,26)}
  val  X = new Button("Exit")       {enabled       = false}
  val bagLabel = new Label("Bag") {preferredSize = new Dimension(45,26)}
  val outputTA = new TextArea      {editable      = false}
  
  
  val top          = new MainFrame {
    title          = "Bag - Subscript"
    location       = new Point    (0,0)
    preferredSize  = new Dimension(300,300)
    contents       = new BorderPanel {
      //add(new FlowPanel(bagLabel, X), BorderPanel.Position.North) 
      add(new FlowPanel(lA, pA, mA, cA), BorderPanel.Position.North) 
      add(new FlowPanel(lB, pB, mB, cB), BorderPanel.Position.Center) 
      add(outputTA, BorderPanel.Position.South) 
    }
  }
  var ca = 0
  var cb = 0
  def dA(d: Int) = {ca+=d; cA.text = ca.toString}
  def dB(d: Int) = {cb+=d; cB.text = cb.toString}
  override def  live = _execute(liveScript)

  override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => bag)}
def bag: subscript.vm.ScriptNode[Any] = subscript.DSL._script[Any](None, Symbol("bag")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => A), subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => bag), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => ax))), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => B), subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => bag), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => bx))))}
def A = subscript.DSL._script[Any](None, Symbol("A")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => pA), subscript.DSL._at[subscript.vm.N_code_tiny[Any], subscript.vm.model.template.concrete.T_code_tiny[Any]](here => {
  implicit val there: subscript.vm.N_code_tiny[Any] = here.there;
gui
}).apply(subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("dA(subscript.DSL._maybeVarCall(\"+1\"))")
}, true)))}
def ax = subscript.DSL._script[Any](None, Symbol("ax")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mA), subscript.DSL._at[subscript.vm.N_code_tiny[Any], subscript.vm.model.template.concrete.T_code_tiny[Any]](here => {
  implicit val there: subscript.vm.N_code_tiny[Any] = here.there;
gui
}).apply(subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("dA(subscript.DSL._maybeVarCall(\"-1\"))")
}, true)))}
def B = subscript.DSL._script[Any](None, Symbol("B")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => pB), subscript.DSL._at[subscript.vm.N_code_tiny[Any], subscript.vm.model.template.concrete.T_code_tiny[Any]](here => {
  implicit val there: subscript.vm.N_code_tiny[Any] = here.there;
gui
}).apply(subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("dB(subscript.DSL._maybeVarCall(\"+1\"))")
}, true)))}
def bx = subscript.DSL._script[Any](None, Symbol("bx")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mB), subscript.DSL._at[subscript.vm.N_code_tiny[Any], subscript.vm.model.template.concrete.T_code_tiny[Any]](here => {
  implicit val there: subscript.vm.N_code_tiny[Any] = here.there;
gui
}).apply(subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("dB(subscript.DSL._maybeVarCall(\"-1\"))")
}, true)))}
}
