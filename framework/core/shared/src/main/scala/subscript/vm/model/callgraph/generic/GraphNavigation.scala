package subscript.vm
package model.callgraph
package generic

import subscript.vm.executor._
import subscript.DSL._

import subscript.vm.model.template.concrete.LogicalKind

trait GraphNavigation {this: CallGraphNode =>

  def n_ary_op_else_ancestor: N_n_ary_op = {
    this match {
      case n:N_n_ary_op => n
      case _            => n_ary_op_ancestor
    }
  }
  // answer the n_ary_op ancestor in case there is one and the path leading thereto does not branch
  def n_ary_op_ancestor = if (parents.size != 1) null else parents.head.n_ary_op_else_ancestor
  def n_ary_op_ancestor_up(n: Int): N_n_ary_op = {
    var ancestor = n_ary_op_ancestor
    if (n<=0 || ancestor==null) return ancestor
    return ancestor.n_ary_op_ancestor_up(n-1)
  }

  def break_up(n: Int): Unit = {
    var ancestor = n_ary_op_ancestor_up(n)
    if (ancestor!=null) ancestor.mustBreak
  }
  def getLogicalKind_n_ary_op_ancestor: LogicalKind.LogicalKindType = {
    val a = n_ary_op_ancestor
    if (a==null) return null
    a.getLogicalKind
  }
}
