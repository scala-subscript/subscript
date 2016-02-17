package subscript

import subscript.language
import subscript.Predef._

import scala.util._

import subscript.vm._
import subscript.DSL._

trait CommonHelpers {
  import subscript.vm._
  import subscript.DSL._

  implicit class ScriptEvaluator[T](n: Script[T]) {
    def e: Try[T] = _execute(n).$
  }

  script..
    n1 = {!1!}
    n2 = {!2!}

}
