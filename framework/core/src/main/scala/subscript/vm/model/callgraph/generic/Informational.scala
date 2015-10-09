package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._

trait Informational {this: CallGraphNode =>
  def basicInfoString = f"$index%2d $template"
  def infoString      = basicInfoString
  override def toString = index+" "+template
}