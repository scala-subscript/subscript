package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

/** Tests the usage of the ScriptDSL methods as intended in the parser. */
class ScriptDSLSuite extends FlatSpec with Matchers
                                      with CommonHelpers
                                      with ScriptDSLSuiteHelpers {

  "Dataflow DSL method" should "work with one 'then' clause" in {
    dataflowTest([success: 2  ]) shouldBe Success(2)
    dataflowTest([success: "2"]) shouldBe Success("2")
  }

  it should "work with exceptions" in {
    dataflowTest([failure: "Something went wrong!"]) shouldBe Success("Runtime Exception")
  }

}

trait ScriptDSLSuiteHelpers {this: CommonHelpers =>
  import subscript.vm._
  import subscript.DSL._

  // The one from the parser
  val sampleFunction: PartialFunction[Any, Script[Any]] = {
    case x: Int    => [success(x)]
    case y: String => [success(y.toString)]
  }

  val sampleExceptionsFunction: PartialFunction[Throwable, Script[Any]] = {
    case e: RuntimeException => [success: "Runtime Exception"]
  }

  def dataflowTest(s: Script[Any]): Try[Any] =
    [ScriptDSL._dataflow(s, sampleFunction, sampleExceptionsFunction)].e

}
