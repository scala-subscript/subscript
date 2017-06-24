// package subscript.vm.model.callgraph.concrete
package subscript.vm

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._
import subscript.vm.model.template.concrete._
import subscript.DSL._

// Loops
case class N_while(template: T_while)
  extends CallGraphLeafNode
  {type T = T_while}

case class N_loop(template: T_loop)
  extends CallGraphLeafNode
  {type T = T_loop}

// Breaks
case class N_break(template: T_break)
  extends CallGraphLeafNode
  {type T = T_break}

case class N_optional_break(template: T_optional_break)
  extends CallGraphLeafNode
  {type T = T_optional_break}

case class N_optional_break_loop(template: T_optional_break_loop)
  extends CallGraphLeafNode
  {type T = T_optional_break_loop}

// Constants
case class N_delta(template: T_delta)
  extends CallGraphLeafNode     
  {type T = T_delta}

case class N_epsilon(template: T_epsilon) 
  extends CallGraphLeafNode     
  {type T = T_epsilon}

case class N_nu(template: T_nu) 
  extends CallGraphLeafNode                               
  {type T = T_nu}
