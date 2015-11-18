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
  val sampleFunction: Any => ScriptNode[Any] = {
    case x: Int    => [success(x)]
    case y: String => [success(y.toString)]
  }

  val sampleExceptionsFunction: Throwable => ScriptNode[Any] = {
    case e: RuntimeException => [success: "Runtime Exception"]
  }

  def dataflowTest(s: ScriptNode[Any]): Try[Any] =
    [ScriptDSL._dataflow(s, sampleFunction, sampleExceptionsFunction)].e

}