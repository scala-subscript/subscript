package subscript.vm.model.callgraph.generic

import subscript.vm.model.callgraph.NodeStateEvent

trait ListenableNode {this: Container =>
  import NodeStateEvent._
  
  def onActivate  : ()=>Unit  = getCodeProperty(OnActivate  )
  def onDeactivate: ()=>Unit  = getCodeProperty(OnDeactivate)
  def onSuspend   : ()=>Unit  = getCodeProperty(OnSuspend   )
  def onResume    : ()=>Unit  = getCodeProperty(OnResume    )
  def onSuccess   : ()=>Unit  = getCodeProperty(OnSuccess   )
  def onFailure   : ()=>Unit  = getCodeProperty(OnFailure   )
  def onExclude   : ()=>Unit  = getCodeProperty(OnExclude   )
  
  def onActivate  (c: =>Unit) = setCodeProperty(OnActivate  , ()=>c)
  def onDeactivate(c: =>Unit) = setCodeProperty(OnDeactivate, ()=>c)
  def onSuspend   (c: =>Unit) = setCodeProperty(OnSuspend   , ()=>c)
  def onResume    (c: =>Unit) = setCodeProperty(OnResume    , ()=>c)
  def onSuccess   (c: =>Unit) = setCodeProperty(OnSuccess   , ()=>c)
  def onFailure   (c: =>Unit) = setCodeProperty(OnFailure   , ()=>c)
  def onExclude   (c: =>Unit) = setCodeProperty(OnExclude   , ()=>c)

  def onActivateOrResume   (c: =>Unit) = {setCodeProperty(OnActivate  , ()=>c); setCodeProperty(OnResume , ()=>c)}
  def onDeactivateOrSuspend(c: =>Unit) = {setCodeProperty(OnDeactivate, ()=>c); setCodeProperty(OnSuspend, ()=>c)}
  def onSuccessOrFailure   (c: =>Unit) = {setCodeProperty(OnSuccess   , ()=>c); setCodeProperty(OnFailure, ()=>c)}
}
