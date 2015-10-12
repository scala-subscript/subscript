package subscript.swing
import subscript.language

import subscript.swing.Scripts._
import subscript.DSL._

import scala.language.implicitConversions

object SubScriptDebugger extends SubScriptDebuggerApp
object SubScriptDebugger2 extends SubScriptDebuggerApp {
  // extra singleton to allow for GraphicalDebugging the SubScriptDebuggerApp
  override def doesThisAllowToBeDebugged = true
}

class SubScriptDebuggerApp extends SimpleSubscriptApplication with GraphicalDebugger {

  override def live: Unit = try _execute(liveScript, debugger=null, myScriptExecutor)
                            catch {case t:Throwable => t.printStackTrace; throw t}

  override def main(args: Array[String]) = super.main(args)

  script..
     liveScript = [
                    {*awaitMessageBeingHandled(true)*}

                    if shouldStep then [
                      @gui: updateDisplay
                      stepCommand || [if autoCheckBox.selected then {*waitForStepTimeout*}]
                    ]

                    {!messageBeingHandled(false)!}
                    ...
                  ]
                  || exitDebugger

   stepCommand  = stepButton
   exitCommand  = exitButton
   exitDebugger = exitCommand @gui:{!exitConfirmed=confirmExit!} while(!exitConfirmed)

}
