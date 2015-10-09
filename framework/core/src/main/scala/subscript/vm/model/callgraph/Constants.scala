package subscript.vm.model.callgraph

object ActivationMode extends Enumeration {
  type ActivationModeType = Value
  val
    Active,
    Optional,
    Inactive
  = Value
}

object ExecutionResult extends Enumeration {
  type ExecutionResultType = Value
  val
    Success,
    Failure,
    Ignore,
    Break,
    OptionalBreak
  = Value
}

object NodeStateEvent extends Enumeration {
  type NodeStateEvent = Value
  val
    OnActivate,
    OnDeactivate,
    OnSuspend,
    OnResume,
    OnSuccess,
    OnFailure,
    OnExclude
  = Value
}
