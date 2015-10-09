package subscript.vm.model.callgraph.generic

import subscript.vm.executor._
import subscript.vm.model.callgraph._
import subscript.DSL._

trait ChildrenState {this: CallGraphNode =>
  var nActivatedChildren = 0 // i.e. including the number of already deactivated children)
  var nActivatedChildrenWithSuccess = 0
  def nActivatedChildrenWithoutSuccess = nActivatedChildren - nActivatedChildrenWithSuccess
  def nActiveChildren = children.size
  def nDeactivatedChildren = nActivatedChildren - nActiveChildren
  
  def childChangesSuccess(child: Child) = {
//val old_nActivatedChildrenWithSuccess = nActivatedChildrenWithSuccess
    nActivatedChildrenWithSuccess += (if (child.hasSuccess) 1 else -1)
//println(f"$this%-14s child $child%-14s changesSuccess to ${child.hasSuccess} nActivatedChildrenWithSuccess: $old_nActivatedChildrenWithSuccess => $nActivatedChildrenWithSuccess")
  }
  
  def extendedInfoString = f"$basicInfoString%.10s S=${hasSuccess} nActivated=${nActivatedChildren} (=${nActivatedChildrenWithSuccess}S+${nActivatedChildrenWithoutSuccess}N)"

  var aChildEndedInFailure = false
  def aChildEndedInSuccess = rightmostChildThatEndedInSuccess_index>=0
  def childThatEndedInSuccess_index(i: Int) = rightmostChildThatEndedInSuccess_index = 
             if (rightmostChildThatEndedInSuccess_index== -1) i else scala.math.max(rightmostChildThatEndedInSuccess_index, i)
                                  
  var rightmostChildThatEndedInSuccess_index = -1
  
}