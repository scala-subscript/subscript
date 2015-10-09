package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import TemplateNode.Child
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._


// Local launches and calls
case class T_launch(
    override val child0: Child
) extends T_1_ary

case class T_launch_anchor(
    override val child0: Child
) extends T_1_ary

case class T_call[R](
             val calleeName: String, // TBD: make symbol ?
    override val code      : N_call[R] => ScriptNode[R],
             val mustPropagateResultValue: Boolean = false
) extends T_0_ary with TemplateCodeHolder[ScriptNode[R],N_call[R]] with ResultPropagator

// Root script types
case class T_script (
    override val owner : AnyRef,
    override val kind  : String,
                 name  : Symbol
) extends TemplateNode with TreeNode_1_def with RootNode {
  var _child0: Child = null
  override def toString = name.name
  override def child0: Child = _child0
  override def children: Seq[Child] = _children
  var _children: Seq[Child] = null
  def setChild(c: Child) = {
    _child0 = c
    _children = child0::Nil
    TreeNode.setIndexes(this, 0, 0)
  }
  
}

//case class T_commscript(
//    override val owner       : AnyRef,
//    override val kind        : String,
//                 communicator: Communicator
//) extends TemplateNode with TreeNode_0 with RootNode {
//  override def toString = super.toString+" "+communicator.name.name
//}

//case class T_communication(
//    override val owner: AnyRef,
//    override val kind : String,
//                 names: Seq[Symbol]
//) extends TemplateNode with TreeNode_0 with RootNode {
//  override def toString = super.toString+" "+names.mkString(",")
//}