package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

class ScriptDSLSuite extends FlatSpec with Matchers
                                      with ScriptDSLSuiteHelpers {

  "Dataflow" should "work" in {
    [success: 2 ~~(x: Int)~~> success: x].e shouldBe Success(2)
  }

}

trait ScriptDSLSuiteHelpers {
  import subscript.vm._
  import subscript.DSL._

  implicit class ScriptNodeEvaluator[T](n: ScriptNode[T]) {
    def e: Try[T] = _execute(n).$
  }

}