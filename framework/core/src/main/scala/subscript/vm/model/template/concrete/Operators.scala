package subscript.vm.model.template.concrete

import subscript.vm.model.template._
import subscript.vm._
import subscript.DSL._
import TemplateNode.Child
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._

/**
 * Annotation
 */
case class T_annotation[CN<:CallGraphNode,CT<:Child](
    override val code: N_annotation[CN,CT] => Unit,
    override val child0: Child
) extends T_1_ary with TemplateCodeHolder[Unit,N_annotation[CN,CT]] {type N = N_annotation[CN,CT]}

// Inconsistency: why T_annotation doesn't extend T_1_ary_op?
/**
 * Unary operator.
 */
case class T_1_ary_op(
    override val kind: String,
    override val child0: Child
) extends T_1_ary


// Conditional boolean operators
case class T_if(
    override val code: N_if => Boolean,
    override val child0: Child
) extends T_1_ary with TemplateCodeHolder[Boolean,N_if] {type N = N_if}

case class T_if_else(
    override val code: N_if_else => Boolean,
    override val child0: Child,
    override val child1: Child
) extends T_2_ary with TemplateCodeHolder[Boolean,N_if_else] {type N = N_if_else}

// Conditional script operators
case class T_do_then     (override val child0: Child, override val child1: Child) extends T_2_ary {type N = N_do_then}
case class T_do_else     (override val child0: Child, override val child1: Child) extends T_2_ary {type N = N_do_else}
case class T_do_then_else(override val child0: Child, override val child1: Child, 
                                                      override val child2: Child) extends T_3_ary {type N = N_do_then_else}

/**
 * Operator of arbitrary arity.
 */
case class T_n_ary_op(
    override val kind: String,
    override val children: Child*
) extends T_n_ary

/**
 * Helpers for T_n_ary_op.
 */
object T_n_ary_op {
  
  def getLogicalKind(t: T_n_ary_op): LogicalKind.LogicalKindType = getLogicalKind(t.kind)
  def getLogicalKind(kind: String): LogicalKind.LogicalKindType = {
    kind match {
      case ";" | "|;" | "||;" | "|;|" 
         | "&&" | "&" | "&&:" | "&:"
         | "=="  | "&~~>"  | "&&~~>"
         | "==:" | "&~~>:" | "&&~~>:"
         | "%"   |  "%/"          => LogicalKind.And
                             
      case "||"  | "|"    | "|~~>"  | "||~~>"
         | "||:" | "|:"   | "|~~>:" | "||~~>:"
         | "|+"  | "|/" 
         | "||+" | "||/" 
         | "|+|" | "|/|" 
         | "+"   | "/" | "%" 
         | "%;"  | "%&"       => LogicalKind.Or                         
      
      case "%/%/"               => LogicalKind.None
      
      case _ => null
    }
  }
  def getExclusiveKind(t: T_n_ary_op): ExclusiveKind.ExclusiveKindType = getExclusiveKind(t.kind)
  def getExclusiveKind(kind: String): ExclusiveKind.ExclusiveKindType = {
    kind match {
      case ";" | "." | "+"  => ExclusiveKind.All                             
      case "/"              => ExclusiveKind.LeftOnly                             
      case "|;|" | "|+|"    => ExclusiveKind.Disambiguating_all
      case "|/|"            => ExclusiveKind.Disambiguating_leftOnly
      case _                => ExclusiveKind.None
    }
  }
  def isMerge(t: T_n_ary_op): Boolean = isMerge(t.kind)
  def isMerge(kind: String): Boolean = {
    kind match {
      case "&&" | "&" | "&&:" | "&:"
         | "==" | "&~~>"  | "&&~~>"  | "|~~>"  | "||~~>"

         | "||"  | "|"  
         | "||;" | "|;"   => true                         
      
      case _ => false
    }
  }
  def isLeftMerge(t: T_n_ary_op): Boolean = isLeftMerge(t.kind)
  def isLeftMerge(kind: String): Boolean = {
    kind match {
      case "&&:" | "&:"
         | "==:" | "&~~>:"  | "&&~~>:"  | "|~~>:"  | "||~~>:"   
         | "||:" | "|:" => true
      case _            => false
    }
  }
  def isSuspending(t: T_n_ary_op): Boolean = isSuspending(t.kind)
  def isSuspending(kind: String): Boolean = {
    kind match {
      case "%" 
         | "%;" | "%&"
         | "%/" | "%/%/" => true
      case _             => false
    }
  }
}

/**
 * Logical kind of operators.
 */
object LogicalKind extends Enumeration {
  type LogicalKindType = Value
  val And, Or, None = Value
}

/**
 * Exclusive kind.
 */
object ExclusiveKind extends Enumeration {
  type ExclusiveKindType = Value
  val All, LeftOnly, None, Disambiguating_all, Disambiguating_leftOnly = Value
}