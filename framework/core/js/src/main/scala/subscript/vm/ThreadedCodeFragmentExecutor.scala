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

package subscript.vm

import scala.util.{Try, Failure}
import scala.collection.mutable._
import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.vm.model.template.TemplateCodeHolder

import scala.language.existentials


class ThreadedCodeFragmentExecutor[R](n: N_code_threaded[R], scriptExecutor: ScriptExecutor[_]) extends NormalCodeFragmentExecutor[R](n, scriptExecutor)  {
  // TODO: Copied from the Normal code fragment execution to avoid threading.

  override def executeAA(lowLevelCodeExecutor: CodeExecutorTrait): Unit = {
    super.executeAA(lowLevelCodeExecutor)
    if (regardStartAndEndAsSeparateAtomicActions) aaHappened(DurationalCodeFragmentStarted)
    doCodeExecutionIn(lowLevelCodeExecutor)
  }
  override def afterExecuteAA_internal = {
    if (!n.isExcluded) {
       aaHappened( if (regardStartAndEndAsSeparateAtomicActions) DurationalCodeFragmentEnded else AtomicCodeFragmentExecuted)
       if (n.hasSuccess) {
         succeeded
       }
       deactivate
    }
  }
}