package subscript.example.life
import subscript.file

import scala.math._
import scala.swing._
import scala.swing.event._
import subscript.Predef._
import subscript.swing.SimpleSubscriptApplication
import subscript.swing.Scripts._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._
import subscript.vm.executor._

object LifeFrame extends LifeFrameApplication
class LifeFrameApplication extends BasicLifeFrameApplication {
  import scala.language.implicitConversions

    //////////////////////////////////////////////
    // speed control
    //////////////////////////////////////////////
    
    def getSleep_ms = pow(2, 12-speed).toInt // logarithmic scale
    
    def sleep = 
      try {
        val sleepPart_ms = 10
        val startTime_ms = System.currentTimeMillis
        while (System.currentTimeMillis - startTime_ms < getSleep_ms) {
          Thread.sleep(sleepPart_ms)
        }
      }
      catch { case e: InterruptedException => /*println("sleep interrupted")*/}

    def sleep_ms(time_ms: Int) = 
      try {
          Thread.sleep(time_ms)
      }
      catch { case e: InterruptedException => /*println("sleep interrupted")*/}

    //////////////////////////////////////////////
    // confirm exit dialog
    //////////////////////////////////////////////
    def confirmExit: Boolean = Dialog.showConfirmation(top.contents.head, "Are you sure?", "About to exit")==Dialog.Result.Yes
     
    board.listenTo(board.mouse.clicks)
    board.listenTo(board.mouse.moves)

    //////////////////////////////////////////////
    // handle MouseDown events
    //////////////////////////////////////////////
     def resetLastMousePos: Unit = board.resetLastMousePos // so that mouseDragSet will initially not draw a line
     
     def handleMouseSingleClick(p: java.awt.Point): Unit = {
       selectedPattern match {
          case None => board.mouseDownToggle(p)
          case Some(s) => val ec = Coord(p.x/board.cellSizeX, p.y/board.cellSizeY)
                          for (pc <- ConwayPatterns.moveTo(s,ec)) {board.setCellValue(pc.x,pc.y,true)}
       }
     }
     def handleMouseMove(p: java.awt.Point): Unit = board.mouseDragSet(p)
     def handleMouseDrag(p: java.awt.Point): Unit = board.mouseDragSet(p)
    
     def char2Value(ac: Any) = {var c=chr(ac); int2Value(c-'0')}
     def int2Value ( i: Int) = if (i==0) maxSpeed else minSpeed+i-1

     // conversion method, unfortunately needed since the SubScript compiler does not handle var types well
     def chr(c:Any) = c.asInstanceOf[Int].toChar
     
  implicit def key(c: subscript.vm.FormalConstrainedParameter[Char]) = subscript.DSL._script[Any](None, Symbol("key"), c.~??(Symbol("c"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => key2(subscript.DSL._maybeVarCall("top"), subscript.DSL._maybeVarCall("subscript.vm.ActualAdaptingParameter(c)")))}
  implicit def vkey(k: subscript.vm.FormalConstrainedParameter[Key.Value]) = subscript.DSL._script[Any](None, Symbol("vkey"), k.~??(Symbol("k"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => vkey2(subscript.DSL._maybeVarCall("top"), subscript.DSL._maybeVarCall("subscript.vm.ActualAdaptingParameter(k)")))}

def randomizeCommand = subscript.DSL._script[Any](None, Symbol("randomizeCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => randomizeButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) =>  'r'))}
def clearCommand = subscript.DSL._script[Any](None, Symbol("clearCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => clearButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) =>  'c'))}
def stepCommand = subscript.DSL._script[Any](None, Symbol("stepCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => stepButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) =>  ' '))}
def exitCommand = subscript.DSL._script[Any](None, Symbol("exitCommand")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => windowClosing(subscript.DSL._maybeVarCall("top"))))}
def multiStepStartCmd = subscript.DSL._script[Any](None, Symbol("multiStepStartCmd")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => startButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => Key.Enter))}
def multiStepStopCmd = subscript.DSL._script[Any](None, Symbol("multiStepStopCmd")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => stopButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => Key.Enter))}
def doExit = subscript.DSL._script[Any](None, Symbol("doExit")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => exitCommand), subscript.DSL._dataflow_then(
  subscript.DSL._script[Any](None, Symbol("~~>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_normal[Any], subscript.vm.model.template.concrete.T_code_normal[Any]](here => {
  implicit val there: subscript.vm.N_code_normal[Any] = here.there;
gui
}).apply(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("confirmExit")
}, true))}
, (_r: Any) => _r match {case r: Boolean => subscript.DSL._script[Any](None, Symbol("<lambda>")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("!r")
})}}
))}
def boardControl = subscript.DSL._script[Any](None, Symbol("boardControl")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._par_or2(subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => noise), subscript.DSL._seq(subscript.DSL._seq(subscript.DSL._optionalBreak_loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => singleStep)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => multiStep))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => clear), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => randomize)))}
def do1Step = subscript.DSL._script[Any](None, Symbol("do1Step")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("board.calculateGeneration")
}, true), subscript.DSL._at[subscript.vm.model.callgraph.CallGraphNode, subscript.vm.model.template.TemplateNode.Child](here => {
  implicit val there: subscript.vm.model.callgraph.CallGraphNode = here.there;
gui
}).apply(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => board.validate)))}
def noise = subscript.DSL._script[Any](None, Symbol("noise")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => 'n'), subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._at[subscript.vm.model.callgraph.CallGraphNode, subscript.vm.model.template.TemplateNode.Child](here => {
  implicit val there: subscript.vm.model.callgraph.CallGraphNode = here.there;
gui
}).apply(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => board.doRandomize())), subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("sleep")
}, true)))}
def randomize = subscript.DSL._script[Any](None, Symbol("randomize")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => randomizeCommand), subscript.DSL._at[subscript.vm.model.callgraph.CallGraphNode, subscript.vm.model.template.TemplateNode.Child](here => {
  implicit val there: subscript.vm.model.callgraph.CallGraphNode = here.there;
gui
}).apply(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => board.doRandomize())))}
def clear = subscript.DSL._script[Any](None, Symbol("clear")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => clearCommand), subscript.DSL._at[subscript.vm.model.callgraph.CallGraphNode, subscript.vm.model.template.TemplateNode.Child](here => {
  implicit val there: subscript.vm.model.callgraph.CallGraphNode = here.there;
gui
}).apply(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => board.doClear)))}
def singleStep = subscript.DSL._script[Any](None, Symbol("singleStep")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => stepCommand), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => do1Step))}
def multiStep = subscript.DSL._script[Any](None, Symbol("multiStep")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => multiStepStartCmd), subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => do1Step), subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("sleep")
}, true)), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => multiStepStopCmd)))}
def speedControl = subscript.DSL._script[Any](None, Symbol("speedControl")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._alt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedKeyInput), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedButtonInput), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedSliderInput)))}
def setSpeed(s: Int) = subscript.DSL._script[Any](None, Symbol("setSpeed")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.model.callgraph.CallGraphNode, subscript.vm.model.template.TemplateNode.Child](here => {
  implicit val there: subscript.vm.model.callgraph.CallGraphNode = here.there;
gui
}).apply(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeedValue(subscript.DSL._maybeVarCall("s"))))}
def speedKeyInput = {
  val c = subscript.DSL._declare[Char](scala.Symbol("c"))
  subscript.DSL._script[Any](None, Symbol("speedKeyInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._while (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("here.pass<10")
}), subscript.DSL._seq(subscript.DSL._val(c, (_node: subscript.vm.N_localvar[Char]) => {implicit val here = _node; val tr: Char = subscript.DSL._maybeVarCall("(subscript.DSL._maybeVarCall(\"pass_up1+'0'\")).toChar"); tr}), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => key(subscript.DSL._maybeVarCall("c"))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeed(subscript.DSL._maybeVarCall("char2Value(subscript.DSL._maybeVarCall(\"c\"))"))))))}
  
}
def speedButtonInput = subscript.DSL._script[Any](None, Symbol("speedButtonInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("speed>minSpeed")
})(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedDecButton)), subscript.DSL._if (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("speed<maxSpeed")
})(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedIncButton)))}
def speedDecButton = subscript.DSL._script[Any](None, Symbol("speedDecButton")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => minSpeedButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeed(subscript.DSL._maybeVarCall("minSpeed")))), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => slowerButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeed(subscript.DSL._maybeVarCall("(subscript.DSL._maybeVarCall(\"speed-1\"))")))))}
def speedIncButton = subscript.DSL._script[Any](None, Symbol("speedIncButton")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._alt(subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => maxSpeedButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeed(subscript.DSL._maybeVarCall("maxSpeed")))), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => fasterButton), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeed(subscript.DSL._maybeVarCall("(subscript.DSL._maybeVarCall(\"speed+1\"))")))))}
def speedSliderInput = subscript.DSL._script[Any](None, Symbol("speedSliderInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedSlider), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => setSpeed(subscript.DSL._maybeVarCall("speedSlider.value"))))}
def mouseInput = subscript.DSL._script[Any](None, Symbol("mouseInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._disrupt(subscript.DSL._par(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseClickInput), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseDragInput)), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => doubleClick), subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseMoveInput), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => doubleClick), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => resetLastMousePos))))), subscript.DSL._loop)}
def mouseClickInput = {
  val p = subscript.DSL._declare[java.awt.Point](scala.Symbol("p"))
  subscript.DSL._script[Any](None, Symbol("mouseClickInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(p, (_node: subscript.vm.N_localvar[java.awt.Point]) => {implicit val here = _node; val tr: java.awt.Point = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseSingleClick(subscript.DSL._maybeVarCall("board"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")"))), subscript.DSL._seq(subscript.DSL._loop, subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => resetLastMousePos), subscript.DSL._disrupt(subscript.DSL._seq(subscript.DSL._threaded[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("sleep_ms(subscript.DSL._maybeVarCall(\"220\"))")
}, true), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => here.break_up(subscript.DSL._maybeVarCall("2")))), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseDoubleClick(subscript.DSL._maybeVarCall("board"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")")))), subscript.DSL._loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => handleMouseSingleClick(subscript.DSL._maybeVarCall("p"))), subscript.DSL._loop)}
  
}
def doubleClick = {
  val p = subscript.DSL._declare[java.awt.Point](scala.Symbol("p"))
  subscript.DSL._script[Any](None, Symbol("doubleClick")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(p, (_node: subscript.vm.N_localvar[java.awt.Point]) => {implicit val here = _node; val tr: java.awt.Point = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseDoubleClick(subscript.DSL._maybeVarCall("board"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")"))))}
  
}
def mouse_Released = {
  val p = subscript.DSL._declare[java.awt.Point](scala.Symbol("p"))
  subscript.DSL._script[Any](None, Symbol("mouse_Released")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(p, (_node: subscript.vm.N_localvar[java.awt.Point]) => {implicit val here = _node; val tr: java.awt.Point = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseReleased(   subscript.DSL._maybeVarCall("board"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")"))))}
  
}
def mouseDragInput = subscript.DSL._script[Any](None, Symbol("mouseDragInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._disrupt(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseDraggings(subscript.DSL._maybeVarCall("board"), (e: MouseEvent) => subscript.DSL._maybeVarCall("handleMouseDrag(subscript.DSL._maybeVarCall(\"e.point\"))"))), subscript.DSL._seq(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouse_Released), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => resetLastMousePos))), subscript.DSL._loop)}
def mouseMoveInput = subscript.DSL._script[Any](None, Symbol("mouseMoveInput")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseMoves(    subscript.DSL._maybeVarCall("board"), (e: MouseEvent) => subscript.DSL._maybeVarCall("handleMouseMove(subscript.DSL._maybeVarCall(\"e.point\"))")))} 
  //mouseMoveToggle = var p:java.awt.Point=null mouseMove(board, p?) // doMouseDraw(p))  

override def liveScript = subscript.DSL._script[Any](None, Symbol("liveScript")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._par_or2(subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => boardControl), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseInput), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => speedControl), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => doExit))}
                   
}


