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

  "Dataflow DSL method" should "work with one 'then' clause" in {
    [ScriptDSL._dataflow_then([success: 2], {
        case x: Int => [success(x * 2)]
      })].e shouldBe Success(4)
  }

}

trait ScriptDSLSuiteHelpers {
  import subscript.vm._
  import subscript.DSL._

  implicit class ScriptNodeEvaluator[T](n: ScriptNode[T]) {
    def e: Try[T] = _execute(n).$
  }

}