package subscript.vm.model.template

import subscript.vm._
import subscript.DSL._
import subscript.vm.model.template.concrete._

/**
 * Helpers for TemplateNode.
 */
object TemplateNode {
  // Defined statically in the object. Now we can
  // use these types everywhere.
  type Root   =  RootNode with TemplateNode
  type Parent =                TemplateNode
  type Child  = ChildNode with TemplateNode
  
  private def caretIf(b:Boolean): String = if (b) "^" else ""
  def kindAsString(t: TemplateNode): String = 
    t match {
      // matching on T_n_ary_op (and T_1_ary_op) does not work;
      // therefore FTTB those classes have their own implementation of kindAsString
      case T_1_ary_op               (kind: String, _) => kind
      case T_n_ary_op               (kind: String, _) => kind
      
      case T_code_normal            (_,doPropagate)   =>    s"{}${caretIf(doPropagate)}"
      case T_code_tiny              (_,doPropagate)   =>  s"{!!}${caretIf(doPropagate)}"
      case T_code_threaded          (_,doPropagate)   =>  s"{**}${caretIf(doPropagate)}"
      case T_code_unsure            (_,doPropagate)   =>  s"{??}${caretIf(doPropagate)}"
      case T_code_eventhandling     (_,doPropagate)   =>   s"{.}${caretIf(doPropagate)}"
      case T_code_eventhandling_loop(_,doPropagate)   => s"{...}${caretIf(doPropagate)}"
      case T_localvar               (isVal: Boolean, 
                                    isLoop: Boolean, 
                                      localVariable,
                                                   _) => "var"
      case T_privatevar             (name: Symbol)    => "private "+name
      case T_while                  (_)               => "while"
      case T_break                  ()                => "break"
      case T_optional_break         ()                => "."
      case T_optional_break_loop    ()                => ".."
      case T_delta                  ()                => "(-)"
      case T_epsilon                ()                => "(+)"
      case T_nu                     ()                => "(+-)"
      case T_loop                   ()                => "..."
      case T_if                     (_,_)             => "if"
      case T_if_else                (_,_,_)           => "if-else"
      case T_launch                 (_)               => "*"
      case T_launch_anchor          (_)               => "**"
      case T_do_then                (_,_)             => "do-then"
      case T_do_else                (_,_)             => "do-else"
      case T_do_then_else           (_,_,_)           => "do-then-else"
      case T_annotation             (_,_)             => "@:"
      case T_local_valueCode        (_, _, _)         => "T_local_valueCode???"
      case T_call                   (calleeName,_,doPropagate)      => calleeName+caretIf(doPropagate)
      case T_script                 (_, kind: String, name: Symbol) => name.toString
    //case T_commscript             [S](_, kind: String, _)                     => "cscript"
    //case T_communication          [S](_, kind: String, names: Seq[Symbol]) => "comm"
      case _ => getClass.getName
    }
  
  def subScriptInfixOpPrecedence(operator: String): Int = // pretty compatible with Scala
    operator.charAt(0) match {
    case ';'             => 1
    case '|'             => 2
    case '^'             => 3
    case '&'             => 4
    case '=' | '!'       => 5
    case '<' | '>'       => 6
    case ':'             => 7
    case '+' | '-'       => 8
    case '*' | '/' | '%' => 9
    case _               => 10
  }
    
  def hierarchyString(thisNode:TemplateNode, parent:TemplateNode, parentIsSpaceSeq: Boolean): String = 
  { 
    val children = thisNode.children
    
    // the next 13 lines seem to be necessary since matching on T_n_ary_op (and T_1_ary_op) does not work
    if (thisNode.isInstanceOf[T_n_ary_op]) {
        val tn = thisNode.asInstanceOf[T_n_ary_op]
        var isSpaceSeq = false
        val doParenthesize = if (parent==null) false 
           else if (parent.isInstanceOf[T_n_ary_op]) {
             val pn = parent.asInstanceOf[T_n_ary_op]
             if (tn.kind==";" && !parentIsSpaceSeq) {isSpaceSeq = true; false}
             else subScriptInfixOpPrecedence(tn.kind) <= subScriptInfixOpPrecedence(pn.kind)
           }
           else parent.isInstanceOf[T_annotation[_,_]] ||
                parent.isInstanceOf[T_1_ary_op]
        val s = children.map(hierarchyString(_, thisNode, isSpaceSeq)).mkString(if (isSpaceSeq) " " else thisNode.kind)
        if (doParenthesize) "(" + s + ")" else s
    }
    else thisNode match {
      case t@T_n_ary_op(kind: String, _) =>
        var isSpaceSeq = false
        val doParenthesize = if (parent==null) false else parent match {
          case t@T_annotation          (_,_)           => true
          case t@T_launch              (_)             => true
          case t@T_launch_anchor       (_)             => true
          case t@T_1_ary_op            (pk: String, _) => true
          case t@T_n_ary_op            (pk: String, _) => if (kind==";" && !parentIsSpaceSeq) {isSpaceSeq = true; false}
                                                          else subScriptInfixOpPrecedence(kind) <= subScriptInfixOpPrecedence(pk)
          case _                                       => false
        }
        val s = children.map(hierarchyString(_, thisNode, isSpaceSeq)).mkString(if (isSpaceSeq) " " else thisNode.kind)
        if (doParenthesize) "(" + s + ")" else s
      case t@T_1_ary_op        (kind: String, _) =>   kind + hierarchyString(children.head, thisNode, false)
      case t@T_launch          (_)     =>       "(*"  + hierarchyString(children.head, thisNode, false) +  "*)"
      case t@T_launch_anchor   (_)     =>       "(**" + hierarchyString(children.head, thisNode, false) + "**)"
      case t@T_if              (_,_)   => "if()then[" + hierarchyString(children.head, thisNode, false) + "]"
      case t@T_if_else         (_,_,_) => "if()then[" + hierarchyString(children.head, thisNode, false) + "]else[" + hierarchyString(children.tail.head, thisNode, false) + "]"
      case t@T_do_then         (_,_)   =>       "do[" + hierarchyString(children.head, thisNode, false) + "]then[" + hierarchyString(children.tail.head, thisNode, false) + "]"
      case t@T_do_else         (_,_)   =>       "do[" + hierarchyString(children.head, thisNode, false) + "]else[" + hierarchyString(children.tail.head, thisNode, false) + "]"
      case t@T_do_then_else    (_,_,_) =>       "do[" + hierarchyString(children.head, thisNode, false) + "]then[" + hierarchyString(children.tail.head, thisNode, false) + 
                                                                                                          "]else[" + hierarchyString(children.tail.tail.head, thisNode, false) + "]"
      case t@T_annotation      (_,_)   => thisNode.kind + hierarchyString(children.head, thisNode, false)
      case t@T_script          (_, kind: String, name: Symbol) => name.toString + " = " + hierarchyString(children.head, thisNode, false)
    //case T_commscript(_, kind: String, _)                     => "cscript"
    //case T_communication(_, kind: String, names: Seq[Symbol]) => "comm"
      case _ => thisNode.kind
    }
  }
}

/**
 * Base class for all template nodes. 
 */
trait TemplateNode extends TreeNode with TemplateNodeHelpers {
  override type Root   = TemplateNode.Root  
  override type Parent = TemplateNode.Parent
  override type Child  = TemplateNode.Child 
  
  def owner: AnyRef = null
  def kind: String = TemplateNode.kindAsString(this)
  override def toString = kind
}

/**
 * Node that can hold code.
 */
trait TemplateCodeHolder[R,N] extends TemplateNode {val code: N => R}
trait ResultPropagator {def mustPropagateResultValue: Boolean}

// Differentiation by arity.
trait T_0_ary extends TemplateNode with TreeNode_0 with ChildNode
trait T_1_ary extends TemplateNode with TreeNode_1 with ChildNode
trait T_2_ary extends TemplateNode with TreeNode_2 with ChildNode
trait T_3_ary extends TemplateNode with TreeNode_3 with ChildNode
trait T_n_ary extends TemplateNode                 with ChildNode

/**
 * Code fragments.
 */
trait T_code_fragment[R,N<:subscript.vm.model.callgraph.N_code_fragment[R]] extends T_0_ary with TemplateCodeHolder[R,N] with ResultPropagator