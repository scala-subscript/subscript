package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

class DataflowSuite extends FlatSpec with Matchers
                                     with CommonHelpers {

  "Dataflow" should "work" in {
    [
      success: 2 ~~(x: Int)~~> {!x!}^
    ].e shouldBe Success(2)
  }

  it should "work with two clauses" in {
    [
      success: "2" ~~(x: Int   )~~> success: x
                  +~~(x: String)~~> success: "Str"
    ].e shouldBe Success("Str")
  }

  it should "work with exceptions clauses" in {
    [
      failure: "Emergency!" ~~(x: Int             )~~> success: x
                           +~~(x: String          )~~> success: "Str"
                          +~/~(e: RuntimeException)~~> success: e.getMessage
    ].e shouldBe Success("Emergency!")
  }

  it should "work with exceptions and non-exceptions clauses given in any order" in {
    [
      failure: "Emergency!" ~~(x: Int                )~~> success: x
                          +~/~(e: java.io.IOException)~~> success: e.getMessage
                           +~~(x: String             )~~> success: "Str"
                          +~/~(e: RuntimeException   )~~> success: "Runtime"
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

  it should "be right associative" in {
    script..
      a = ^1
      b = ^2

    ([
      a ~~(x: Int)~~> b ~~(y: Int)~~> ^(x + y)
    ]).e shouldBe Success(3)
  }

  it should "be right associative with extra clauses correctly" in {
    script..
      a = ^1
      b = ^"2"

    ([
      a ~~(x: Int)~~> b ~~(y: Int   )~~> ^(x + y)
                       +~~(z: String)~~> ^(x + z.toInt)
    ]).e shouldBe Success(3)
  }

  it should "not throw an exception if lhs's `$` is `null`: $success should be `null` then" in {
    script a = {!!} {!!}

    ([
      a ~~(x: Int)~~> ^1
       +~~(null  )~~> ^2
    ]).e shouldBe Success(2)
  }

  it should "not activate its else part in case it is excluded" in {
    var i = 0
    script a = {..} ~~(x: Int)~~> let i = 1
                  +~/~(null  )~~> let i = 2
    ([
      a || {!!}
    ]).e

    i shouldBe 0
  }

  it should "store its `then` and `else` partial functions in the `do-then-else` operator" in {
    import subscript.vm.model.callgraph.CallGraphTreeNode
    import subscript.vm.Script

    ([
      var definedAt1  = false
      var definedAt11 = false
      
      {!
        val fun = here.ancestor(5).getProperty[String, PartialFunction[Any, Script[Any]]]("then").get
        definedAt1  = fun.isDefinedAt(1 )
        definedAt11 = fun.isDefinedAt(11)
        11
      !}^ ~~(x: Int if x > 10)~~> [+]
      
      ^definedAt1^^1
      ^definedAt11^^2

    ]).e shouldBe Success((false, true))
  }

  "Dataflow map" should "work with pattern matches" in {
    [n1 ~~(r: Int)~~^ (r * 2)].e shouldBe(Success(2))
  }

  it should "work with multiple patterns" in {
    [
      n2 ~~(r: Int if r <  0)~~^ (-r * 2)
        +~~(r: Int if r >= 0)~~^ ( r * 2)
    ].e shouldBe(Success(4))
  }

  it should "work in a shortened version" in {
    def triple(x: Int) = x * 3
    ([n2 ~~^ triple]).e shouldBe Success(6)
  }

}
