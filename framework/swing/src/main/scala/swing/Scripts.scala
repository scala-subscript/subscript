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
import subscript.language


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
  
 
 implicit script ..  // TBD: handle tabs in scanner so that line position becomes reasonable
   stateChange(slider: Slider)                   = event(SliderStateChangedReactor[Any,N_code_eventhandling[Any]](slider))
   clicked(button:AbstractButton)                = event(           ClickedReactor[Any,N_code_eventhandling[Any]](button))

 script ..   // TBD: add @gui: annotations
  event[E <: Event] (reactor:ScriptReactor[Any,N_code_eventhandling[Any]], ?e: E) =  @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: {. e = reactor.currentEvent.asInstanceOf[E] .}
  event (reactor:ScriptReactor[Any,N_code_eventhandling[Any]]) =  @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: {.     .}
  event_loop(reactor:ScriptReactor[Any,N_code_eventhandling_loop[Any]], task: MouseEvent=>Unit)   =  @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: 
                                                                                            {... task.apply(reactor.currentEvent.asInstanceOf[MouseEvent]) ...}
  event_loop_KTE(reactor:ScriptReactor[Any,N_code_eventhandling_loop[Any]], task: KeyTyped=>Unit)   = @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: 
                                                                                            {... task.apply(reactor.currentEvent.asInstanceOf[KeyTyped]) ...}
  // TBD: MouseEvent should become type parameter, as in the following (which does not compile)
  //event_loop[E<:Event](reactor:Reactor[Unit,N_code_eventhandling_loop[Unit]], task: E=>Unit)   = @{reactor.subscribe(there); there.onDeactivate{reactor.unsubscribe}; there.onSuccess{reactor.acknowledgeEventHandled}}: 
  //                                                                                                {... task.apply(reactor.currentEvent.asInstanceOf[E]) ...}
       anyEvent(comp: Component)                           = event(          AnyEventReactor[Any,N_code_eventhandling[Any]](comp))                                                    
    windowClosing(window: Window)                          = event(     WindowClosingReactor[Any,N_code_eventhandling[Any]](window))
// mouseClicks   (comp: Component, task: MouseEvent=>Unit) = event_loop( MouseClickedReactor[Any,N_code_eventhandling_loop[Any]](comp), task)
   mousePresses  (comp: Component, task: MouseEvent=>Unit) = event_loop( MousePressedReactor[Any,N_code_eventhandling_loop[Any]](comp), task)
   mouseDraggings(comp: Component, task: MouseEvent=>Unit) = event_loop( MouseDraggedReactor[Any,N_code_eventhandling_loop[Any]](comp), task)
   mouseMoves    (comp: Component, task: MouseEvent=>Unit) = event_loop(   MouseMovedReactor[Any,N_code_eventhandling_loop[Any]](comp), task)
   mouseReleases (comp: Component, task: MouseEvent=>Unit) = event_loop(MouseReleasedReactor[Any,N_code_eventhandling_loop[Any]](comp), task)

   mouseSingleClick (comp: Component, ?p : java.awt.Point) = mouseClicks(1, comp, ?p) // TBD: "p?"
   mouseDoubleClick (comp: Component, ?p : java.awt.Point) = mouseClicks(2, comp, ?p)
   mouseTripleClick (comp: Component, ?p : java.awt.Point) = mouseClicks(3, comp, ?p)
   mouseClicks(n:Int,comp: Component, ?p : java.awt.Point) = var mce: MouseClicked=null 
                                                             event(MouseClickedReactor[Any,N_code_eventhandling[Any]](comp, n), ?mce ) // TBD ...
                                                             { p=mce.point }
   mouseMove        (comp: Component, ?p : java.awt.Point) = var mme: MouseMoved=null 
                                                             event(   MouseMovedReactor[Any,N_code_eventhandling[Any]](comp), ?mme ) // TBD: "mme?" instead of "ActualOutputParameter(...)"
                                                             { p=mme.point }
   mousePressed     (comp: Component, ?p : java.awt.Point) = var mpe: MousePressed=null
                                                             event(   MousePressedReactor[Any,N_code_eventhandling[Any]](comp), ?mpe ) // TBD: ...
                                                             { p=mpe.point }
   mouseReleased    (comp: Component, ?p : java.awt.Point) = var mre: MouseReleased=null
                                                             event(   MouseReleasedReactor[Any,N_code_eventhandling[Any]](comp), ?mre ) // TBD: ...
                                                             { p=mre.point }

/* these 4 scripts should become:
   mouseClicks(n:Int,comp: Component, ?p : java.awt.Point) = var mce: MouseClicked =null; event( MouseClickedReactor[Any,N_code_eventhandling[Any]](comp, n), ?mce); {! p=mce.point !}
   mouseMove        (comp: Component, ?p : java.awt.Point) = var mme: MouseMoved   =null; event(   MouseMovedReactor[Any,N_code_eventhandling[Any]](comp   ), ?mme); {! p=mme.point !}
   mousePressed     (comp: Component, ?p : java.awt.Point) = var mpe: MousePressed =null; event( MousePressedReactor[Any,N_code_eventhandling[Any]](comp   ), ?mpe); {! p=mpe.point !}
   mouseReleased    (comp: Component, ?p : java.awt.Point) = var mre: MouseReleased=null; event(MouseReleasedReactor[Any,N_code_eventhandling[Any]](comp   ), ?mre); {! p=mre.point !}

for now:

error: missing parameter type for expanded function ((x$4) => _mce.at(here).value = x$4)
mouseClicks(n:Int,comp: Component, ?p : java.awt.Point) = var mce: MouseClicked =null; event( MouseClickedReactor[Any,N_code_eventhandling[Any]](comp, n), ?mce); {! p=mce.point !}
                                                                                                                                                   ^
*/

     guard(comp: Component, test: () => Boolean)           = if test() then .. else ...
                                                             anyEvent(comp)

     key2(publisher: Publisher, ??keyCode : Char     )     = event(         KeyTypedReactor[Any,N_code_eventhandling[Any]](publisher, ??keyCode ))
    vkey2(publisher: Publisher, ??keyValue: Key.Value)     = event(        VKeyTypedReactor[Any,N_code_eventhandling[Any]](publisher, ??keyValue))
    
     keyEvent2 (publisher: Publisher, ??keyTypedEvent : KeyTyped)  = event(         KeyTypedEventReactor [Any,N_code_eventhandling[Any]](publisher, ??keyTypedEvent))
     keyEvents2(publisher: Publisher, task: KeyTyped=>Unit)        = event_loop_KTE(KeyTypedEventsReactor[Any,N_code_eventhandling_loop[Any]](publisher), task)

}
