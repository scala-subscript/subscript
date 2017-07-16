package subscript.vm.model.callgraph.generic

import subscript.vm.model.callgraph._

trait State {this: CallGraphNode =>
  // Success flag
  private var _hasSuccess = false
  def hasSuccess = _hasSuccess
  def hasSuccess_=(value: Boolean) {
    if (_hasSuccess == value) return
    _hasSuccess = value
//println(s"$this changesSuccess to ${hasSuccess} parent: ${if (parents.isEmpty) "-" else parents(0)}")
    
    forEachParent(p => p childChangesSuccess this)
  }
  
  /** Exclusion flag */
  var isExcluded = false
  
  def isExecuting  = false
  var numberOfBusyActions = 0
  def isActionBusy = numberOfBusyActions>0
  var aaHappenedCount = 0
  
  var stamp = 0
  
  def asynchronousAllowed: Boolean = false
}
