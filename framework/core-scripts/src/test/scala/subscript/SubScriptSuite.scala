package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

/** Tests various SubScript syntax. */
class SubScriptSuite extends FlatSpec with Matchers
                                      with CommonHelpers
                                      with SubScriptSuiteHelpers {

  "Dataflow" should "work" in {
    [success: 2 ~~(x: Int)~~> success: x].e shouldBe Success(2)
  }

  it should "work with two clauses" in {
    [success: "2" ~~(x: Int   )~~> success: x
                 +~~(x: String)~~> success: "Str"
    ].e shouldBe Success("Str")
  }

  it should "work with exceptions clauses" in {
    [failure: "Emergency!" ~~(x: Int   )~~> success: x
                          +~~(x: String)~~> success: "Str"
                          +~/~(e: RuntimeException)~~> success: e.getMessage
    ].e shouldBe Success("Emergency!")
  }

  it should "work with exceptions and non-exceptions clauses given in any order" in {
    [
      failure: "Emergency!" ~~(x: Int   )~~> success: x
                          +~/~(e: java.io.IOException)~~> success: e.getMessage
                           +~~(x: String)~~> success: "Str"
                          +~/~(e: RuntimeException)~~> success: "Runtime"
    ].e shouldBe Success("Runtime")
  }

  it should "work with patterns" in {
    [
      success: (1, 2) ~~((x: Int, y: Int))~~> success(x + y)
    ].e shouldBe Success(3)
  }

  it should "work with if-containing patterns and @'s" in {
    [
      success: (1, 2) ~~(p @ (x: Int, y: Int) if x == 2)~~> success: 1
                     +~~(p @ (x: Int, y: Int) if x < 2 )~~> success: p._1
    ].e shouldBe Success(1)
  }

}

trait SubScriptSuiteHelpers {

}