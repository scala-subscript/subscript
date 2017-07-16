// package subscript.vm.model.callgraph.concrete
package subscript.vm

import scala.util.Try
import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._

// N_code_tiny has separate failed field, since hasSuccess is often false for behaving neutrally in Or-like contexts
case class N_code_tiny     [R](template: T_code_tiny    [R]) extends N_code_fragment[R] {type T = T_code_tiny    [R]; var failed: Boolean = false; 
                                                                                              override def fail: Unit = {failed=true; super.fail}}
case class N_code_normal   [R](template: T_code_normal  [R]) extends N_code_fragment[R] {type T = T_code_normal  [R]}
case class N_code_threaded [R](template: T_code_threaded[R]) extends N_code_fragment[R] {type T = T_code_threaded[R]}
case class N_code_unsure   [R](template: T_code_unsure  [R]) extends N_code_fragment[R]
                                                             with UnsureExecutionResult {type T = T_code_unsure[R]  }

case class N_code_eventhandling     [R](template: T_code_eventhandling     [R]) extends N_code_fragment[R] 
                                                                                with UnsureExecutionResult {type T = T_code_eventhandling[R]
  def eventHappened = codeExecutor.executeAA
  // experimental
  def executeForTry(tryResult: Try[R]): Unit = codeExecutor.asInstanceOf[EventHandlingCodeFragmentExecutor[R]].executeForTry(tryResult)
}
case class N_code_eventhandling_loop[R](template: T_code_eventhandling_loop[R]) extends N_code_fragment[R]
                                                                                with    LoopExecutionResult{type T = T_code_eventhandling_loop[R]}
