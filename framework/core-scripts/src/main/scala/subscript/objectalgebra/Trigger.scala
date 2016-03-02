package subscript.objectalgebra

import subscript.language

class Trigger extends SSProcess {
  var listeners = List[() => Unit]()
  def trigger = listeners.foreach(_())
  def addListener(f: () => Unit) {listeners ::= f}

  script live = @{
    val listener = {() => there.codeExecutor.executeAA}
    addListener(listener)
    there.onDeactivate {listeners = listeners.filter(_ != listener)}
  }: {. .}
}
