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
import subscript.language

import scala.util.{Try,Success,Failure}
import subscript.vm._
import subscript.DSL._
import subscript.vm.executor._
import subscript.vm.model.template._
import subscript.vm.model.template.concrete._
import subscript.vm.model.callgraph._
import subscript.vm.model.callgraph.generic._

// Predefined stuff - pass and some scripts: times, delta, epsilon, nu
//
object Predef {
  
  def `$`         [R]               (implicit s: Script[R]): Try[R]    = s.$
  def `$success`  [R]               (implicit s: Script[R]): R         = s.$ match {case Success(s) => s}
  def `$failure`  [R]               (implicit s: Script[R]): Throwable = s.$ match {case Failure(f) => f case null => null}
  def `$_=`       [R] (v: Try[R]   )(implicit s: Script[R])            = {s.$=v; v match {case Failure(_) => s.fail /*; println("$=Failure(_)")*/ case _ => }}
  def `$success_=`[R] (v: R        )(implicit s: Script[R])            = {s.$=Success(v)}
  def `$failure_=`[R] (v: Throwable)(implicit s: Script[R])            = {s.$=Failure(v); s.fail /*; println("$failure_=")*/}

  def pass    (implicit node: CallGraphTreeNode): Int = node.pass
  def pass_up1(implicit node: CallGraphTreeNode): Int = node.n_ary_op_ancestor.pass
  def pass_up2(implicit node: CallGraphTreeNode): Int = node.n_ary_op_ancestor.n_ary_op_ancestor.pass

  /* type mismatch errors in these hand-compiled versions:
  def _times(   n:Int) = _script(this, 'times)     {(script:Script[Any]) => _while{_node=>{implicit val here=_node; pass<n}}}
  def _break_up(n:Int) = _script(this, 'break_up)  {(script:Script[_]) => _tiny{ (_node:N_code_tiny[Unit])=>{implicit val here=_node; here.break_up(n)}}}
  def _break_up1       = _script(this, 'break_up1) {(script:Script[Unit]) => _call("break_up", (_node:N_call[Unit])=>{implicit val here=_node; _break_up(1)})}
  def _break_up2       = _script(this, 'break_up2) {(script:Script[Unit]) => _call("break_up", (_node:N_call[_])=>{implicit val here=_node; _break_up(2)})}
  */

  script..
    times(n:Int) = while(here.pass<n) // TBD: make here implicit
      
    delta        = [-]
    epsilon      = [+]
    nu           = [+-]
    
//    break_up(n:Int) = {!here.break_up(n)!}
//    break_up1 = break_up(1)
//    break_up2 = break_up(2)

}
