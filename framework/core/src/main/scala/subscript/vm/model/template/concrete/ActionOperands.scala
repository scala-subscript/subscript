package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._

case class T_code_normal            [R](override val code: N_code_normal            [R] => R, val mustPropagateResultValue: Boolean = false) extends T_code_fragment[R,N_code_normal            [R]] {type N = N_code_normal            [R]}
case class T_code_tiny              [R](override val code: N_code_tiny              [R] => R, val mustPropagateResultValue: Boolean = false) extends T_code_fragment[R,N_code_tiny              [R]] {type N = N_code_tiny              [R]}
case class T_code_threaded          [R](override val code: N_code_threaded          [R] => R, val mustPropagateResultValue: Boolean = false) extends T_code_fragment[R,N_code_threaded          [R]] {type N = N_code_threaded          [R]}
case class T_code_unsure            [R](override val code: N_code_unsure            [R] => R, val mustPropagateResultValue: Boolean = false) extends T_code_fragment[R,N_code_unsure            [R]] {type N = N_code_unsure            [R]}
case class T_code_eventhandling     [R](override val code: N_code_eventhandling     [R] => R, val mustPropagateResultValue: Boolean = false) extends T_code_fragment[R,N_code_eventhandling     [R]] {type N = N_code_eventhandling     [R]}
case class T_code_eventhandling_loop[R](override val code: N_code_eventhandling_loop[R] => R, val mustPropagateResultValue: Boolean = false) extends T_code_fragment[R,N_code_eventhandling_loop[R]] {type N = N_code_eventhandling_loop[R]}
