package subscript.objectalgebra

import subscript.language

class Trigger extends SSProcess {
  var listeners = List[() => Unit]()
  def addListener(f: () => Unit) {listeners ::= f}

  private[this] var value: Any = null.asInstanceOf[Any]
  
  /** Sets the value and triggers the trigger. `v` will be return as a result value of this trigger. */
  def trigger(v: Any) {
    value = v
    listeners.foreach(_())
  }

  /** Triggers this trigger with `null` as a result value. */
  def trigger {trigger(null.asInstanceOf[Any])}

  override script live = @{
    val listener = {() => there.codeExecutor.executeAA}
    addListener(listener)
    there.onDeactivate {listeners = listeners.filter(_ != listener)}
  }: {.value.}

}
