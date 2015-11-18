package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

/** Tests the usage of the ScriptDSL methods as intended in the parser. */
class ScriptDSLSuite extends FlatSpec with Matchers
                                      with ScriptDSLSuiteHelpers {

  "Dataflow" should "work" in {
    [success: 2 ~~(x: Int)~~> success: x].e shouldBe Success(2)
  }

  "Dataflow DSL method" should "work with one 'then' clause" in {
    dataflowTest([success: 2  ]) shouldBe Success(2)
    dataflowTest([success: "2"]) shouldBe Success("2")
  }

  it should "not break when the result is not set at all" in {
    dataflowTest([{:"I do nothing":}]) shouldBe Success("Nothing!")
  }

  it should "work with exceptions" in {
    dataflowTest([failure: "Something went wrong!"]) shouldBe Success("Runtime Exception")
  }

}

trait ScriptDSLSuiteHelpers {
  import subscript.vm._
  import subscript.DSL._
  
  implicit class ScriptNodeEvaluator[T](n: ScriptNode[T]) {
    def e: Try[T] = _execute(n).$
  }

  // The one from the parser
  val sampleFunction: Try[Any] => ScriptNode[Any] = {
    case Success(x: Int)    => [success(x)]
    case Success(y: String) => [success(y.toString)]

    case Failure(e: RuntimeException) => [success("Runtime Exception")]

    case null => [success: "Nothing!"]
  }

  def dataflowTest(s: ScriptNode[Any]): Try[Any] =
    [ScriptDSL._dataflow_then(s, sampleFunction)].e

}