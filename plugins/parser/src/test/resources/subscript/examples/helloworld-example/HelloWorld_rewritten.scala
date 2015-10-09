package subscript.example
import subscript.file

import subscript.DSL._

// Subscript sample application: "Hello world!", printed using a sequence of 2 code fragments

object HelloWorld {
  // bridge method:
  def main( args: Array[String]): Unit = _execute(live)
  
  // The compiler translates this internally to a method:
  //
  //  def _live() = _script(this, 'main, _args~'args) {_seq({print("Hello ")}, {println("world!") })}
def live = {
  val x = subscript.DSL._declare[Int](scala.Symbol("x"))
  subscript.DSL._script[Any](None, Symbol("live")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._val(x, (_node: subscript.vm.N_localvar[Int]) => {implicit val here = _node; val tr: Int = subscript.DSL._maybeVarCall("3"); tr}), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("println(subscript.DSL._maybeVarCall(\"x\"))")
}, true))}
  
}
 
}