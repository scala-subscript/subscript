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

package subscript

// Scala library
import scala.language.implicitConversions
import scala.collection.mutable.LinkedList

// Scala macros
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

// VM
import subscript.vm._
import subscript.vm.executor._

// Template
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import TemplateNode.Child

// Call graph
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._

/*
 * Internal Scala DSL for SubScript.
 * Using this DSL one can make SubScript programs without the need 
 * for a compiler that understands the specific SubScript syntax. 
 * 
 * Also this DSL may well be the target for a SubScript extension to the Scala compiler.
 * 
 * Usage: see example programs
 */
object DSL {
  def _script[S](owner:AnyRef, name:Symbol, p: FormalParameter[_]*)(childTemplateAt: (Script[S])=>TemplateNode.Child): ScriptNode[S] = {
    // In order to create the Script, we need to know T_script, the tempalte
    // To create the template, we should not need to know its children
    // because to create the children, we need to know Script first
    
    val template = T_script(owner, "script", name)
    val result: ScriptNode[S] = new ScriptNode(template, p:_*)
    template.setChild(childTemplateAt(result))
    result
  }
  //def _comscript(owner : AnyRef, communicator: Communicator, p: FormalParameter[_]*)                       : Script[Unit] = {(_c: N_call) => _c.calls(T_commscript(owner, "communicator" , communicator), p:_*)}
  
// TBD: communication scripts
//  def _communication(owner: Any, names: Symbol*): N_communication => TemplateNode = {
//    (_c: N_communication) => _c.inits(T_communication("communication", names.toList.map(_.asInstanceOf[Symbol])), owner)
//  }
//  def _communication(owner: Any, names: Symbol*)(_body: N_communication => TemplateNode) = { 
//    (_c: N_communication) => _c.inits(T_communication("communication", names.toList.map(_.asInstanceOf[Symbol])), owner)
//  }

  def getScriptTemplate    [S](s: ScriptNode[S]): T_script     = s.template // TBD: check; was: {val nc = N_call(T_call("", null)); s(nc); nc.t_callee}
  def getScriptBodyTemplate[S](s: ScriptNode[S]): TemplateNode = getScriptTemplate(s).child0
  def toScriptString       [S](s: ScriptNode[S]): String       = getScriptTemplate(s).hierarchyString
  def toScriptBodyString   [S](s: ScriptNode[S]): String       = {val c = getScriptBodyTemplate(s); if(c==null) "" else c.hierarchyString}
//def _communication(body: N_communication => TemplateNode) = Communication(body)
//def _communicator(name: Symbol) = Communicator(name)
//def _relate(communication: Communication, crs: CommunicatorRole*): Unit = communication.setCommunicatorRoles(crs.toList)

//implicit def communicatorToCommunicatorRole(c: Communicator) = new CommunicatorRole(c)
  
  def _execute[S     ](_script: ScriptNode[S]                             ): ScriptExecutor[S] = _execute(_script, null, true)
  def _execute[S<:X,X](_script: ScriptNode[S], executor: ScriptExecutor[X]): ScriptExecutor[X] = _execute(_script, null, executor)
  def _execute[S     ](_script: ScriptNode[S], debugger: MsgListener      ): ScriptExecutor[S] = _execute(_script, debugger, false)
  def _execute[S     ](_script: ScriptNode[S], allowDebugger: Boolean     ): ScriptExecutor[S] = _execute(_script, null, allowDebugger)
  def _execute[S     ](_script: ScriptNode[S], debugger: MsgListener
                                         , allowDebugger: Boolean     ): ScriptExecutor[S] = {
    val executor = ScriptExecutorFactory.createScriptExecutor[S](allowDebugger && debugger == null)
    _execute(_script, debugger, executor)
  }
  def _execute[S<:X,X](_script: ScriptNode[S], debugger: MsgListener, executor: ScriptExecutor[X]): ScriptExecutor[X] = {
    if (debugger!=null) debugger.attach(executor)
    
    try {
      executor.run(_script)
    }
    catch {case t:Throwable => println(s"DSL._execute caught throwable: $t"); throw t}
  }

  def _normal             [R](cf: N_code_normal            [R] =>R, mustPropagateResultValue: Boolean) = T_code_normal            (cf, mustPropagateResultValue)
  def _threaded           [R](cf: N_code_threaded          [R] =>R, mustPropagateResultValue: Boolean) = T_code_threaded          (cf, mustPropagateResultValue)
  def _unsure             [R](cf: N_code_unsure            [R] =>R, mustPropagateResultValue: Boolean) = T_code_unsure            (cf, mustPropagateResultValue)
  def _tiny               [R](cf: N_code_tiny              [R] =>R, mustPropagateResultValue: Boolean) = T_code_tiny              (cf, mustPropagateResultValue)
  def _eventhandling      [R](cf: N_code_eventhandling     [R] =>R, mustPropagateResultValue: Boolean) = T_code_eventhandling     (cf, mustPropagateResultValue)
  def _eventhandling_loop [R](cf: N_code_eventhandling_loop[R] =>R, mustPropagateResultValue: Boolean) = T_code_eventhandling_loop(cf, mustPropagateResultValue)

   // alternative code fragment variations that have no "here" parameter
   // handy for "manual use", i.e. not compiler generated
  def _normal0            [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_normal            ((_here:N_code_normal            [R]) => cf, mustPropagateResultValue)
  def _threaded0          [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_threaded          ((_here:N_code_threaded          [R]) => cf, mustPropagateResultValue)
  def _unsure0            [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_unsure            ((_here:N_code_unsure            [R]) => cf, mustPropagateResultValue)
  def _tiny0              [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_tiny              ((_here:N_code_tiny              [R]) => cf, mustPropagateResultValue)
  def _eventhandling0     [R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_eventhandling     ((_here:N_code_eventhandling     [R]) => cf, mustPropagateResultValue)
  def _eventhandling_loop0[R](cf: => R, mustPropagateResultValue: Boolean = false) = T_code_eventhandling_loop((_here:N_code_eventhandling_loop[R]) => cf, mustPropagateResultValue)

  def _call      [R](calleeName: String, code: N_call[R] => ScriptNode[R], mustPropagateResultValue: Boolean = false) = T_call[R](calleeName, code, mustPropagateResultValue)
  
  implicit def valueToActualValueParameter[T<:Any](value: T) = new ActualValueParameter(value)

  //def _at[N<:CallGraphNode,T<:Child](_cf:N=>Unit)  
  //= (_child: T) => T_annotation[N,T]((here:N_annotation[N,T]) => _cf(here.there), _child)
 
  def _at[N<:CallGraphNode,T<:Child](_cf:N_annotation[N,T]=>Unit)
  = (_child: T) => T_annotation[N,T]((here:N_annotation[N,T]) => _cf(here), _child)
 
  def _declare[T](name: Symbol) = new LocalVariable[T](name)
  
  // local variables need to be declared explicitly first; usage is as in:
  //  implicit def _key(_publisher: FormalInputParameter[Publisher], _keyCode: FormalConstrainedParameter[Char])  = {
  //    val _r = _declare[KeyPressScriptReactor[N_code_eh]]('r)      // <<== declaration
  //    _script('key, _publisher~'publisher, _keyCode~??'keyCode) {
  //     _seq( 
  //       _val(_r, (here:N_localvar[_]) => new KeyPressScriptReactor[N_code_eh](_publisher.value, _keyCode)),  // <<== initialisation
  //       _at{(there:N_code_eh) => {_r.at(there).value.subscribe(there); there.onDeactivate{_r.at(there).value.unsubscribe}; 
  //                                                                      there.onSuccess   {_r.at(there).value.acknowledgeEventHandled}}}
  //          (_eventhandling{})//{println("\nKey"+_keyCode.value)} // Temporary tracing
  //     )
  //    }
  //  }
  
  def _var     [V](v: LocalVariable[V]                             ) = T_localvar(false, false, v, null)
  def _var_loop[V](v: LocalVariable[V]                             ) = T_localvar(false,  true, v, null)
  def _var     [V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar(false, false, v, valueCode)
  def _val     [V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar( true, false, v, valueCode)
  def _var_loop[V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar(false,  true, v, valueCode)
  def _val_loop[V](v: LocalVariable[V], valueCode: N_localvar[V]=>V) = T_localvar( true,  true, v, valueCode)

  def _privatevar[T<:Any](vsym: Symbol) = T_privatevar(vsym)

  // variants for operators with 0 to many operands
  //def _op0(opSymbol: String)                                                                      = T_0_ary(opSymbol)
  //def _op1(opSymbol: String)(c0: ChildNode)                                               = T_1_ary(opSymbol, c0)
  //def _op2(opSymbol: String)(c0: ChildNode, c1: ChildNode)                        = T_2_ary(opSymbol, c0, c1)
  //def _op3(opSymbol: String)(c0: ChildNode, c1: ChildNode, c2: ChildNode) = T_3_ary(opSymbol, c0, c1, c2)

  /* the following does not function well, as of Scala 2.10.
   * See https://issues.scala-lang.org/browse/SI-4176
   *
  def _op (opSymbol: String)(children: ChildNode*)                                        = T_n_ary(opSymbol, children:_*)
  
  def _seq               = _op(";")_
  def _alt               = _op ("+")_
  def _par               = _op ("&")_
  def _par_or            = _op ("|")_
  def _par_and2          = _op ("&&")_
  def _par_or2           = _op ("||")_
  def _par_equal         = _op ("==")_
  def _disrupt           = _op ("/")_
  def _shuffle           = _op ("%")_
  def _shuffle_1_or_more = _op ("%%")_
  def _seq_1_or_more     = _op (";%;")_
  def _interrupt         = _op ("%/")_
  def _interrupt_0_or_more = _op ("%/%/")_
  */
  
  def _op1(opSymbol: String)(child0  : Child ) = T_1_ary_op(opSymbol, child0)
  def _op (opSymbol: String)(children: Child*) = T_n_ary_op(opSymbol, children:_*)
  
  def _seq                (children: Child*) = _op(";"   )(children:_*)
  def _alt                (children: Child*) = _op("+"   )(children:_*)
  def _par                (children: Child*) = _op("&"   )(children:_*)
  def _par_or             (children: Child*) = _op("|"   )(children:_*)
  def _par_and2           (children: Child*) = _op("&&"  )(children:_*)
  def _par_or2            (children: Child*) = _op("||"  )(children:_*)
  def _par_equal          (children: Child*) = _op("=="  )(children:_*)
  def _disrupt            (children: Child*) = _op("/"   )(children:_*)
  def _shuffle            (children: Child*) = _op("%"   )(children:_*)
  def _shuffle_1_or_more  (children: Child*) = _op("%%"  )(children:_*)
  def _seq_1_or_more      (children: Child*) = _op(";%;" )(children:_*)
  def _interrupt          (children: Child*) = _op("%/"  )(children:_*)
  def _interrupt_0_or_more(children: Child*) = _op("%/%/")(children:_*)
  
  
  def _not           = _op1("!")_
  def _not_react     = _op1("-")_
  def _react         = _op1("~")_
  def _launch        = (child0: Child) => T_launch       (child0)
  def _launch_anchor = (child0: Child) => T_launch_anchor(child0)

  def _empty                                = T_epsilon            ()
  def _deadlock                             = T_delta              ()
  def _neutral                              = T_nu                 ()
  def _break                                = T_break              ()
  def _optionalBreak                        = T_optional_break     ()
  def _optionalBreak_loop                   = T_optional_break_loop()
  def _loop                                 = T_loop               ()
  def _while0  (_cond:         =>Boolean)   = T_while((here: N_while ) => _cond)
  def _while   (_cond:N_while  =>Boolean)   = T_while(_cond)
  def _if0     (_cond:         =>Boolean)(c0: Child) = T_if((here: N_if) => _cond, c0)
  def _if      (_cond:N_if     =>Boolean)(c0: Child) = T_if(_cond, c0)
  def _if_else0(_cond:         =>Boolean)(c0: Child, c1: Child) = T_if_else((here: N_if_else) => _cond, c0, c1)
  def _if_else (_cond:N_if_else=>Boolean)(c0: Child, c1: Child) = T_if_else(_cond, c0, c1)
  
  def _do_then     (c0: Child, c1: Child           )  = T_do_then     (c0, c1) 
  def _do_else     (c0: Child, c1: Child           )  = T_do_else     (c0, c1) 
  def _do_then_else(c0: Child, c1: Child, c2: Child)  = T_do_then_else(c0, c1, c2) 

  class MacroHelpers[C <: Context](val ctx: C) {
    import ctx.universe._

    def evaluate[T](e: ctx.Expr[T]): T = {
      val untyped = ctx.Expr(ctx.untypecheck(e.tree.duplicate))
      ctx.eval(untyped)
    }

    def stringToTree(str: ctx.Expr[String]): Tree = {
      val code = evaluate(str)
      ctx.parse(code)
    }

    def isInfixException(t: Tree): Boolean = {
      val exceptionFunctions = List(q"subscript.vm.ActualAdaptingParameter")

      t match {
        case q"$f(..$args)" if exceptionFunctions.exists(_.equalsStructure(f)) => true
        case _ => false
      }
    }

    // Search the first element of the path like "a.b.c" and transform it
    def processInfix(x: Tree): Tree = x match {
      case e if isInfixException(e) => e  // Some infix trees should not be processed

      case q"$left.$right"      => q"${processInfix(left)}.$right"
      case q"$left.$f($arg)"    => q"${processInfix(left)}.$f(${processInfix(arg)})"
      case q"$left.$f(..$args)" => q"${processInfix(left)}.$f(..$args)"

      case t @ q"$f(..$args)" =>
        val typed = ctx.typecheck(f, silent = true)
        typed match {
          case EmptyTree => t
          case other     => val tpe = typed.tpe
                  if (tpe <:< typeOf[Null                         ]) typed
            else  if (tpe <:< typeOf[LocalVariable             [_]]) q"$typed.at(here).value(..$args)"
            else  if (tpe <:< typeOf[FormalConstrainedParameter[_]]
                   || tpe <:< typeOf[FormalOutputParameter     [_]]) q"$typed.value(..$args)"
            else t
        }

      case other =>
        val typed = ctx.typecheck(other, silent = true)
        typed match {
          case EmptyTree  => other   // Maybe it is a package or a static call of a Java class
          case _          => val tpe = typed.tpe
                  if (tpe <:< typeOf[Null                         ]) typed
            else  if (tpe <:< typeOf[LocalVariable             [_]]) q"$typed.at(here).value"
            else  if (tpe <:< typeOf[FormalConstrainedParameter[_]]
                   || tpe <:< typeOf[FormalOutputParameter     [_]]) q"$typed.value"
            else typed
        }
    }
  
    def relink(x: Tree): Tree = ctx.parse(showCode(x))
  }

  def _maybeCall                (calleeName:        String , expr:        Any ):        Child  = macro _maybeCallImpl
  def _maybeCallImpl(c: Context)(calleeName: c.Expr[String], expr: c.Expr[Any]): c.Expr[Child] = {
    val helper = new MacroHelpers[c.type](c)
    import helper._
    import c.universe._

    val tpe = c.typecheck(expr.tree).tpe.typeArgs.last

    // Following cases of the callee type are possible:
    // 1. Script        - call it with a _call
    // 2. Unit          - wrap it in   a _normal
    // 3. LocalVariable - extract its value and rerun the macro with that value
    // 4. Other         - try to find an implicit conversion to Script.
    //                    Will try to search:
    //                      X => Script
    //                      ActualValueParameter[X] => Script
    //                    Where X is the callee type
    val tree =
      if (tpe <:< typeOf[Script[_]]) q"""
        subscript.DSL._call[Any](${calleeName.tree}, ((here: subscript.vm.N_call[Any]) => {
          val s: subscript.vm.ScriptNode[Any] = ${expr.tree}(here)
          here.calls(s.template, (s.p: _*));
          s
        }), true)
      """        

      else if (tpe <:< typeOf[LocalVariable[_]]) expr.tree match {case q"($here) => $body" =>
        val exprTree = relink(q"($here) => subscript.DSL._maybeVarCall(${showCode(body)})")
        q"subscript.DSL._maybeCall(${calleeName.tree}, $exprTree)"
      }

      else if (tpe =:= typeOf[Unit])
        q"subscript.DSL._tiny({(here: subscript.vm.N_code_tiny[Any]) => ${expr.tree}(here)}, true)"

      else expr.tree match {case q"($here) => $body" =>
        // Try several trees and take the first one that has an implicit conversion to Script
        List(body, q"subscript.vm.ActualValueParameter($body)").toStream  // toStream, so that typecheck is applied lazily and don't waste resources
          .map  {c.typecheck(_, pt = typeOf[Script[_]], silent = true)}   // if an implicit conversion is found, typecheck will produce a tree with this conversion applied explicitly
          .find {_ != EmptyTree}
          .map  {callBody =>
            val call = c.parse(showCode(q"($here) => $callBody"))         // replace the raw body with the callBody - the result of an implicit converison
            q"subscript.DSL._maybeCall(${calleeName.tree}, $call)"        // now that the $call is a `here.type => Script`, apply the macro again so that the script is called
          }
          .getOrElse(body)                                                // if no conversion was found, output raw body to get a proper error
      }

    c.Expr[Child](tree)
  }

  def _maybeVarCall                (expr:        String ):        Any  = macro _maybeVarCallImpl
  def _maybeVarCallImpl(c: Context)(expr: c.Expr[String]): c.Expr[Any] = {
    val helper = new MacroHelpers[c.type](c)
    import c.universe._
    import helper._

    c.Expr[Any] { processInfix(stringToTree(expr)) }
  }



  def _maybeVarAssignment                (lhs:        String , rhs:        Any) :        Any  = macro _maybeVarAssignmentImpl
  def _maybeVarAssignmentImpl(c: Context)(lhs: c.Expr[String], rhs: c.Expr[Any]): c.Expr[Any] = {
    val helper = new MacroHelpers[c.type](c)
    import c.universe._
    import helper._

    val lhsProcessed: Tree = processInfix(stringToTree(lhs))

    c.Expr[Any](q"$lhsProcessed = $rhs")
  }

  def _actualOutputParameter                (expr:        String ):        Any = macro _actualOutputParameterImpl
  def _actualOutputParameterImpl(c: Context)(expr: c.Expr[String]): c.Expr[Any] = {
    val helper = new MacroHelpers[c.type](c)
    import c.universe._
    import helper._

    val target = processInfix(stringToTree(expr))
    val tpe    = c.typecheck(target).tpe

    val assignment = q"(x1: $tpe) => $target = x1"

    c.Expr {q"""
      subscript.vm.ActualOutputParameter[$tpe]($target, $assignment)
    """}
  }

  def _declareNoType    [T               ]            (expr:        T , name:        Symbol ):        LocalVariable[T]  = macro _declareNoTypeImpl[T]
  def _declareNoTypeImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T], name: c.Expr[Symbol]): c.Expr[LocalVariable[T]] = {
    import c.universe._

    c.Expr[LocalVariable[T]] {q"subscript.DSL._declare[${weakTypeOf[T]}]($name)"}
  }

  def _valueCodeNoType    [T               ]            (expr:        T ):        N_localvar[T] => T  = macro _valueCodeNoTypeImpl[T]
  def _valueCodeNoTypeImpl[T: c.WeakTypeTag](c: Context)(expr: c.Expr[T]): c.Expr[N_localvar[T] => T] = {
    import c.universe._

    c.Expr[N_localvar[T] => T] {q"""(_node: subscript.vm.N_localvar[${weakTypeOf[T]}]) => {
      implicit val here = _node
      ${expr.tree}
    }"""}
  }
  
 }
