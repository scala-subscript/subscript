package subscript

import scala.util._

import subscript.vm._
import subscript.DSL._

trait CommonHelpers {

  implicit class ScriptNodeEvaluator[T](n: ScriptNode[T]) {
    def e: Try[T] = _execute(n).$
  }
  
}
