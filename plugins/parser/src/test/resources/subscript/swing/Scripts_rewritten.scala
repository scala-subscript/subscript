/*
    This file is part of Subscript - an extension of the Scala language 
                                     with constructs from Process Algebra.

    Subscript is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License and the 
    GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    Subscript consists partly of a "virtual machine". This is a library; 
    Subscript applications may distribute this library under the 
    GNU Lesser General Public License, rather than under the 
    GNU General Public License. This way your applications need not 
    be made Open Source software, in case you don't want to.

    Subscript is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You may have received a copy of the GNU General Public License
    and the GNU Lesser General Public License along with Subscript.
    If not, see <http://www.gnu.org/licenses/>
*/

package subscript.swing
import subscript.file


import scala.swing._
import scala.swing.event._
import subscript._
import subscript.DSL._
import subscript.Predef._
import subscript.vm._
import subscript.vm.executor._
import subscript.vm.model.callgraph._


/*
 * SimpleSubscriptApplication: a SimpleSwingApplication
 * with an abstract "live" script that is started in a new thread
 */
abstract class SimpleSubscriptApplication extends SimpleSwingApplication{
  override def startup(args: Array[String]) {
    super.startup(args)
    new Thread{override def run={live;quit}}.start()
  }

  def  liveScript: Script[Any]
  def  live: Unit
}

/*
 * Scripts for GUI event handling: mouse down, mouse moves, keys, virtual keys, window events
 * Also a method to acquire a CodeExecutor for processing in the swing thread
 */
object Scripts {

  import scala.language.implicitConversions
  
  def gui1[N<:CallGraphNode](there:N) = {there.adaptExecutor(new SwingCodeExecutorAdapter[Unit, CodeExecutorTrait])}
  def gui [N<:CallGraphNode](implicit there:N)  = {there.asInstanceOf[CallGraphNode]adaptExecutor(new SwingCodeExecutorAdapter[Unit, CodeExecutorTrait])}
  // note that in some cases the type of the "there" parameter is inferred as Any.
  // This is for the time being a workaround:
  // only at the typing phase a refinement call is resoled into either a script call or a method call (=> code fragment)
  // so only then the type of there should be determined.
  // this would require some rework of the compiler: generating code for @...: should only be done in the typer phase
  
//def gui1[N<:CallGraphNode[_]](implicit n:N) = {n.adaptExecutor(new SwingCodeExecutorAdapter[CodeExecutorTrait])}             

	private class SwingCodeExecutorAdapter[R,CE<:CodeExecutorTrait] extends CodeExecutorAdapter[R,CE]{
	  def n = adaptee.n
	  def scriptExecutor = adaptee.scriptExecutor
	
	  override def      executeAA: Unit = {
	    adaptee.regardStartAndEndAsSeparateAtomicActions = adaptee.asynchronousAllowed
	    adaptee.executeAA(this) // Not to be called? TBD: clean up class/trait hierarchy
	  }
	  override def afterExecuteAA_internal: Unit = adaptee.afterExecuteAA_internal  // TBD: clean up class/trait hierarchy so that this def can be ditched
	  override def    interruptAA: Unit = adaptee.interruptAA     // TBD: clean up class/trait hierarchy so that this def can be ditched
	  override def doCodeExecution[R](code: =>R): R = {
	
	    // we need here the default value for R (false, 0, null or a "Unit")
	    // for some strange reason, the following line would go wrong:
	    //
	    // var result: R = _
	    //
	    // A solution using a temporary class was found at
	    // http://missingfaktor.blogspot.com/2011/08/emulating-cs-default-keyword-in-scala.html
	    var result: R = null.asInstanceOf[R]
	    // luckily we have the default value for type R now...
	
	    if (adaptee.asynchronousAllowed) {
	      var runnable = new Runnable {
	        def run(): Unit = {result = adaptee.doCodeExecution(code)}
	      }
	      javax.swing.SwingUtilities.invokeLater(runnable)
	    }
	    else {
	      var runnable = new Runnable {
	        def run(): Unit = {result = adaptee.doCodeExecution(code)}
	      }
	      javax.swing.SwingUtilities.invokeAndWait(runnable)
	    }
	    result
	  }
	}
  
  /*
   * A registry for the ScriptExecutor for which the most recent GUI event had been consumed
   * Note: such executors may invoke one another
   */
  object ScriptReactor {
    var scriptExecutorThatConsumedEvent: ScriptExecutor[_] = null // event.consume not available for button clicks; this consumedEvent item is a workaround
  }
  /*
   * An extension on scala.swing.Reactor that supports event handling scripts in Subscript
   * Allows an event handling script to subscribe and unsubscribe to events
   */
  abstract class ScriptReactor[R,N<:N_code_fragment[R]] extends Reactor {
    def publisher:Publisher
    var executor: EventHandlingCodeFragmentExecutor[R] = _
    def execute = executeMatching(true)
    def executeMatching(isMatching: Boolean): Unit = executor.executeMatching(isMatching)
    val publisher1 = publisher // needed in subclass since publisher does not seem to be accessible
    private var myEnabled = false
    def enabled = myEnabled
    def enabled_=(b:Boolean) = {myEnabled=b}
    // acknowledgeEventHandled is done when an Event Handling Code Fragment succeeds, performed by the ScriptExecutor
    def acknowledgeEventHandled = {ScriptReactor.scriptExecutorThatConsumedEvent = null} 
    
    val listenedEvent: Event
    var currentEvent : Event = null
    def reaction: PartialFunction[Event,Unit] = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case event if (ScriptReactor.scriptExecutorThatConsumedEvent != executor.scriptExecutor) => {
                 execute
                 if (executor.n.hasSuccess) {
                   ScriptReactor.scriptExecutorThatConsumedEvent = executor.scriptExecutor
                   consumeEvent
                 }
               }
    }
    def consumeEvent = {}
    
    def subscribe(n: N): Unit = {
      executor = new EventHandlingCodeFragmentExecutor(n, n.scriptExecutor)
      n.codeExecutor = executor
      val wasAlreadyEnabled = enabled
      publisher.reactions += reaction;
      if (!wasAlreadyEnabled) {enabled=true}
    }
    def canDisableOnUnsubscribe = true
    def unsubscribe: Unit = {
      publisher.reactions -= reaction
      if (canDisableOnUnsubscribe && !publisher.reactions.isDefinedAt(listenedEvent)) {enabled=false}
    }
  }
  
  /*
   * A Reactor that has a swing Component as a Publisher. 
   * This automatically enables and disables the component
   */
  abstract class EnablingReactor[R,N<:N_code_fragment[R]](publisher:Publisher with Component, autoEnableComponent: Boolean = true) extends ScriptReactor[R,N] {
    override def enabled_=(b:Boolean) = {
      super.enabled_=(b)
      if (autoEnableComponent) publisher.enabled = b
    }
  }

  // Note: the following classes have much code in common.
  // This is caused by a dependency on partial functions.
  // In principle this should be cleaned up, but it is not yet clear how that can be done.
  
  // a ComponentReactor for any events
  case class AnyEventReactor[R,N<:N_code_fragment[R]](comp:Component) extends EnablingReactor[R, N](comp) {
    def publisher = comp
    val listenedEvent: Event = null
    override def canDisableOnUnsubscribe = false
    private var myReaction: PartialFunction[Event,Unit] = {case e => currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class WindowClosingReactor[R,N<:N_code_fragment[R]](w:Window) extends ScriptReactor[R,N] {
    def publisher = w
    val listenedEvent: WindowClosing = null
    override def canDisableOnUnsubscribe = false
    private var myReaction: PartialFunction[Event,Unit] = {case e: WindowClosing => currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class SliderStateChangedReactor[R,N<:N_code_fragment[R]](s:Slider) extends ScriptReactor[R,N] {
    def publisher = s
    val listenedEvent: ValueChanged = null
    override def canDisableOnUnsubscribe = false
    private var myReaction: PartialFunction[Event,Unit] = {case e: ValueChanged => currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  
  /*
   * A Reactor that has a swing Component as a Publisher. 
   */
  abstract class ComponentReactor[R,N<:N_code_fragment[R]](comp:Component) extends ScriptReactor[R,N] {
    def publisher = comp
    override def canDisableOnUnsubscribe = false
  }
  
  // TBD: compact these classes someventhandlingow...
  case class MouseClickedReactor[R,N<:N_code_fragment[R]](comp:Component, forClicksCount: Int = 0) extends ComponentReactor[R,N](comp) {
    val listenedEvent: MouseClicked = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseClicked =>
      if (forClicksCount==0 
      ||  forClicksCount==e.clicks) {currentEvent=e; execute; currentEvent=null}}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MousePressedReactor[R,N<:N_code_fragment[R]](comp:Component) extends ComponentReactor[R,N](comp) {
    val listenedEvent: MousePressed = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MousePressed =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MouseReleasedReactor[R,N<:N_code_fragment[R]](comp:Component) extends ComponentReactor[R,N](comp) {
    val listenedEvent: MouseReleased = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseReleased =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MouseMovedReactor[R,N<:N_code_fragment[R]](comp:Component) extends ComponentReactor[R,N](comp) {
    val listenedEvent: MouseMoved = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseMoved =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  case class MouseDraggedReactor[R,N<:N_code_fragment[R]](comp:Component) extends ComponentReactor[R,N](comp) {
    val listenedEvent: MouseDragged = null
    private var myReaction: PartialFunction[Event,Unit] = {case e: MouseDragged =>
      currentEvent=e; execute; currentEvent=null}
    override def reaction: PartialFunction[Event,Unit] = myReaction
  }
  
  /*
   * A EnablingReactor for clicked events on a button
   * TBD: a way to consume clicked events on the button
   */
  case class ClickedReactor[R,N<:N_code_fragment[R]](button:AbstractButton) extends EnablingReactor[R,N](button) {
    val wasFocusable = button.focusable
    override def enabled_=(b:Boolean) = {
      super.enabled_=(b)
      button.focusable = wasFocusable
    }
    def publisher = button
    val listenedEvent: Event = ButtonClicked(button)
    override def consumeEvent = {
      listenedEvent match {
        case ie: InputEvent => ie.consume // unfortunately, this is not applicable
        case _ => // no consume event option seems to be available
    } }
  }
 
  /*
   * A Reactor for key typed events
   */
  case class KeyTypedReactor[R,N<:N_code_fragment[R]](publisher:Publisher, keyCode: FormalConstrainedParameter[Char]) extends ScriptReactor[R,N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case KeyTyped(comp, char, keyModifiers, keyLocationValue) => 
        if (char < 256) {
	      if (keyCode.matches(char)) {
	        keyCode.value = char
	        executeMatching(true)
	      }
        }
    }
    override def unsubscribe: Unit = {
      publisher1.reactions -= reaction
    }
  }
  
  /*
   * A Reactor for virtual key press events
   */
  case class VKeyTypedReactor[R,N<:N_code_fragment[R]](publisher:Publisher, keyValue: FormalConstrainedParameter[Key.Value]) extends ScriptReactor[R,N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case KeyPressed(comp, keyPressedValue, keyModifiers, keyLocationValue) => 
        if (keyValue.matches(keyPressedValue)) {
          keyValue.value = keyPressedValue
          executeMatching(true)
        }
    }
    override def unsubscribe: Unit = {
      publisher1.reactions -= reaction
    }
  }

  case class KeyTypedEventReactor[R,N<:N_code_fragment[R]](publisher:Publisher, keyTypedEvent: FormalConstrainedParameter[KeyTyped]) extends ScriptReactor[R,N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {
      case kt@KeyTyped(comp, char, keyModifiers, keyLocationValue) =>
        if (keyTypedEvent.matches(kt)) {
          keyTypedEvent.value = kt
          executeMatching(true)
        }
    }
    override def unsubscribe: Unit = {publisher1.reactions -= reaction}
  }
  case class KeyTypedEventsReactor[R,N<:N_code_fragment[R]](publisher:Publisher) extends ScriptReactor[R,N] {
    val listenedEvent = null
    override def reaction = myReaction
    private val myReaction: PartialFunction[Event,Unit] = {case e: KeyTyped =>
      currentEvent=e; execute; currentEvent=null
    }
    override def unsubscribe: Unit = {publisher1.reactions -= reaction}
  }

 /*
  * The following subscript code has manually been compiled into Scala; see below
    The redirections to the swing thread using "@gui:" are needed 
    because enabling and disabling the button etc must there be done
  */
  
 
 implicit def stateChange(slider: Slider) = subscript.DSL._script[Any](None, Symbol("stateChange")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(subscript.DSL._maybeVarCall("SliderStateChangedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"slider\"))")))}
 implicit def clicked(button: AbstractButton) = subscript.DSL._script[Any](None, Symbol("clicked")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(           subscript.DSL._maybeVarCall("ClickedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"button\"))")))}

def event[E <: Event](reactor: ScriptReactor[Any,N_code_eventhandling[Any]], e: subscript.vm.FormalOutputParameter[E]) = subscript.DSL._script[Any](None, Symbol("event[E <: Event]"), e.~?(Symbol("e"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_eventhandling[Any], subscript.vm.model.template.concrete.T_code_eventhandling[Any]](here => {
  implicit val there: subscript.vm.N_code_eventhandling[Any] = here.there;
subscript.DSL._maybeVarCall("reactor.subscribe(subscript.DSL._maybeVarCall(\"there\"))"); subscript.DSL._maybeVarCall("there.onDeactivate{subscript.DSL._maybeVarCall(\"reactor.unsubscribe\")}"); subscript.DSL._maybeVarCall("there.onSuccess{subscript.DSL._maybeVarCall(\"reactor.acknowledgeEventHandled\")}")
}).apply(subscript.DSL._eventhandling[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"e\", subscript.DSL._maybeVarCall(\"reactor.currentEvent.asInstanceOf[E]\"))")
}, true))}
def event(reactor: ScriptReactor[Any,N_code_eventhandling[Any]]) = subscript.DSL._script[Any](None, Symbol("event")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_eventhandling[Any], subscript.vm.model.template.concrete.T_code_eventhandling[Any]](here => {
  implicit val there: subscript.vm.N_code_eventhandling[Any] = here.there;
subscript.DSL._maybeVarCall("reactor.subscribe(subscript.DSL._maybeVarCall(\"there\"))"); subscript.DSL._maybeVarCall("there.onDeactivate{subscript.DSL._maybeVarCall(\"reactor.unsubscribe\")}"); subscript.DSL._maybeVarCall("there.onSuccess{subscript.DSL._maybeVarCall(\"reactor.acknowledgeEventHandled\")}")
}).apply(subscript.DSL._eventhandling[Any] (_node => {
  implicit val here = _node

}, true))}
def event_loop(reactor: ScriptReactor[Any,N_code_eventhandling_loop[Any]], task: MouseEvent=>Unit) = subscript.DSL._script[Any](None, Symbol("event_loop")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_eventhandling_loop[Any], subscript.vm.model.template.concrete.T_code_eventhandling_loop[Any]](here => {
  implicit val there: subscript.vm.N_code_eventhandling_loop[Any] = here.there;
subscript.DSL._maybeVarCall("reactor.subscribe(subscript.DSL._maybeVarCall(\"there\"))"); subscript.DSL._maybeVarCall("there.onDeactivate{subscript.DSL._maybeVarCall(\"reactor.unsubscribe\")}"); subscript.DSL._maybeVarCall("there.onSuccess{subscript.DSL._maybeVarCall(\"reactor.acknowledgeEventHandled\")}")
}).apply(subscript.DSL._eventhandling_loop[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("task.apply(subscript.DSL._maybeVarCall(\"reactor.currentEvent.asInstanceOf[MouseEvent]\"))")
}, true))}
def event_loop_KTE(reactor: ScriptReactor[Any,N_code_eventhandling_loop[Any]], task: KeyTyped=>Unit) = subscript.DSL._script[Any](None, Symbol("event_loop_KTE")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._at[subscript.vm.N_code_eventhandling_loop[Any], subscript.vm.model.template.concrete.T_code_eventhandling_loop[Any]](here => {
  implicit val there: subscript.vm.N_code_eventhandling_loop[Any] = here.there;
subscript.DSL._maybeVarCall("reactor.subscribe(subscript.DSL._maybeVarCall(\"there\"))"); subscript.DSL._maybeVarCall("there.onDeactivate{subscript.DSL._maybeVarCall(\"reactor.unsubscribe\")}"); subscript.DSL._maybeVarCall("there.onSuccess{subscript.DSL._maybeVarCall(\"reactor.acknowledgeEventHandled\")}")
}).apply(subscript.DSL._eventhandling_loop[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("task.apply(subscript.DSL._maybeVarCall(\"reactor.currentEvent.asInstanceOf[KeyTyped]\"))")
}, true))}
def anyEvent(comp: Component) = subscript.DSL._script[Any](None, Symbol("anyEvent")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(          subscript.DSL._maybeVarCall("AnyEventReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"comp\"))")))}
def windowClosing(window: Window) = subscript.DSL._script[Any](None, Symbol("windowClosing")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(     subscript.DSL._maybeVarCall("WindowClosingReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"window\"))")))}
def mousePresses(comp: Component, task: MouseEvent=>Unit) = subscript.DSL._script[Any](None, Symbol("mousePresses")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event_loop( subscript.DSL._maybeVarCall("MousePressedReactor[Any,N_code_eventhandling_loop[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("task")))}
def mouseDraggings(comp: Component, task: MouseEvent=>Unit) = subscript.DSL._script[Any](None, Symbol("mouseDraggings")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event_loop( subscript.DSL._maybeVarCall("MouseDraggedReactor[Any,N_code_eventhandling_loop[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("task")))}
def mouseMoves(comp: Component, task: MouseEvent=>Unit) = subscript.DSL._script[Any](None, Symbol("mouseMoves")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event_loop(   subscript.DSL._maybeVarCall("MouseMovedReactor[Any,N_code_eventhandling_loop[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("task")))}
def mouseReleases(comp: Component, task: MouseEvent=>Unit) = subscript.DSL._script[Any](None, Symbol("mouseReleases")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event_loop(subscript.DSL._maybeVarCall("MouseReleasedReactor[Any,N_code_eventhandling_loop[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("task")))}
def mouseSingleClick(comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = subscript.DSL._script[Any](None, Symbol("mouseSingleClick"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseClicks(subscript.DSL._maybeVarCall("1"), subscript.DSL._maybeVarCall("comp"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")")))}
def mouseDoubleClick(comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = subscript.DSL._script[Any](None, Symbol("mouseDoubleClick"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseClicks(subscript.DSL._maybeVarCall("2"), subscript.DSL._maybeVarCall("comp"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")")))}
def mouseTripleClick(comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = subscript.DSL._script[Any](None, Symbol("mouseTripleClick"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => mouseClicks(subscript.DSL._maybeVarCall("3"), subscript.DSL._maybeVarCall("comp"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"p\")")))}
def mouseClicks(n: Int, comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = {
  val mce = subscript.DSL._declare[MouseClicked](scala.Symbol("mce"))
  subscript.DSL._script[Any](None, Symbol("mouseClicks"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(mce, (_node: subscript.vm.N_localvar[MouseClicked]) => {implicit val here = _node; val tr: MouseClicked = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(subscript.DSL._maybeVarCall("MouseClickedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"comp\"), subscript.DSL._maybeVarCall(\"n\"))"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"mce\")") )), subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"p\",subscript.DSL._maybeVarCall(\"mce.point\"))")
}, true))}
  
}
def mouseMove(comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = {
  val mme = subscript.DSL._declare[MouseMoved](scala.Symbol("mme"))
  subscript.DSL._script[Any](None, Symbol("mouseMove"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(mme, (_node: subscript.vm.N_localvar[MouseMoved]) => {implicit val here = _node; val tr: MouseMoved = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(   subscript.DSL._maybeVarCall("MouseMovedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"mme\")") )), subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"p\",subscript.DSL._maybeVarCall(\"mme.point\"))")
}, true))}
  
}
def mousePressed(comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = {
  val mpe = subscript.DSL._declare[MousePressed](scala.Symbol("mpe"))
  subscript.DSL._script[Any](None, Symbol("mousePressed"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(mpe, (_node: subscript.vm.N_localvar[MousePressed]) => {implicit val here = _node; val tr: MousePressed = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(   subscript.DSL._maybeVarCall("MousePressedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"mpe\")") )), subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"p\",subscript.DSL._maybeVarCall(\"mpe.point\"))")
}, true))}
  
}
def mouseReleased(comp: Component, p: subscript.vm.FormalOutputParameter[java.awt.Point]) = {
  val mre = subscript.DSL._declare[MouseReleased](scala.Symbol("mre"))
  subscript.DSL._script[Any](None, Symbol("mouseReleased"), p.~?(Symbol("p"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._var(mre, (_node: subscript.vm.N_localvar[MouseReleased]) => {implicit val here = _node; val tr: MouseReleased = subscript.DSL._maybeVarCall("null"); tr}), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(   subscript.DSL._maybeVarCall("MouseReleasedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"comp\"))"), subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\"mre\")") )), subscript.DSL._tiny[Any] (_node => {
  implicit val here = _node
 subscript.DSL._maybeVarCall("subscript.DSL._maybeVarAssignment(\"p\",subscript.DSL._maybeVarCall(\"mre.point\"))")
}, true))}
  
}
def guard(comp: Component, test: () => Boolean) = subscript.DSL._script[Any](None, Symbol("guard")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._if_else (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("test()")
})(subscript.DSL._optionalBreak_loop, subscript.DSL._loop), subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => anyEvent(subscript.DSL._maybeVarCall("comp"))))}
def key2(publisher: Publisher, keyCode: subscript.vm.FormalConstrainedParameter[Char]) = subscript.DSL._script[Any](None, Symbol("key2"), keyCode.~??(Symbol("keyCode"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(         subscript.DSL._maybeVarCall("KeyTypedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"publisher\"), subscript.DSL._maybeVarCall(\"subscript.vm.ActualAdaptingParameter(keyCode)\") )")))}
def vkey2(publisher: Publisher, keyValue: subscript.vm.FormalConstrainedParameter[Key.Value]) = subscript.DSL._script[Any](None, Symbol("vkey2"), keyValue.~??(Symbol("keyValue"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(        subscript.DSL._maybeVarCall("VKeyTypedReactor[Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"publisher\"), subscript.DSL._maybeVarCall(\"subscript.vm.ActualAdaptingParameter(keyValue)\"))")))}
def keyEvent2(publisher: Publisher, keyTypedEvent: subscript.vm.FormalConstrainedParameter[KeyTyped]) = subscript.DSL._script[Any](None, Symbol("keyEvent2"), keyTypedEvent.~??(Symbol("keyTypedEvent"))){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event(         subscript.DSL._maybeVarCall("KeyTypedEventReactor [Any,N_code_eventhandling[Any]](subscript.DSL._maybeVarCall(\"publisher\"), subscript.DSL._maybeVarCall(\"subscript.vm.ActualAdaptingParameter(keyTypedEvent)\"))")))}
def keyEvents2(publisher: Publisher, task: KeyTyped=>Unit) = subscript.DSL._script[Any](None, Symbol("keyEvents2")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => event_loop_KTE(subscript.DSL._maybeVarCall("KeyTypedEventsReactor[Any,N_code_eventhandling_loop[Any]](subscript.DSL._maybeVarCall(\"publisher\"))"), subscript.DSL._maybeVarCall("task")))}

}