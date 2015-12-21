package subscript.objectalgebra

import subscript.language

class Trigger extends SSProcess {
  var listeners = List[() => Unit]()
  def trigger = listeners.foreach(_())
  def addListener(f: () => Unit) {listeners ::= f}

  script live = @{addListener {() => there.codeExecutor.executeAA}}: {. .}
}
