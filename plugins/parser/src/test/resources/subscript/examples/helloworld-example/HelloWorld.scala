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
  script live = val x: Int = 3
                {println(x)}
 
}