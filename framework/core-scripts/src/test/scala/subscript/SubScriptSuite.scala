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


  "Caret" should "work with code blocks" in {
    [
      {!1!}^ {!2!}
    ].e shouldBe Success(1)
  }

  it should "work with script calls" in {
    [
      n1 n2^
    ].e shouldBe Success(2)
  }

  it should "work with parenthesised code" in {
    [
      {!10!} [n1^ n2]^
    ].e shouldBe Success(1)
  }

  it should "work in prefix position before the literals" in {
    [^1 n2      ].e shouldBe Success(1    )
    [n1 ^"foo" n2].e shouldBe Success("foo")
    [n1 n2 ^true ].e shouldBe Success(true )
  }

  it should "work in prefix position before tuples" in {
    [n1 ^(1, 2) n2].e shouldBe Success((1, 2))
  }

  it should "work in prefix position before expressions" in {
    [n1 ^(2 + 3) n2].e shouldBe Success(5)
  }

  it should "work in prefix position before vars" in {
    [
      var x: Int = 3
      ^x
    ].e shouldBe Success(3)
  }

  "Double caret" should "work with code blocks" in {
    [times: 5 {!here.pass!}^^].e shouldBe Success((0 to 4).toSeq)
  }

  it should "work with script calls" in {
    [times: 5 n1^^].e shouldBe Success((0 to 4).map(x => 1).toSeq)
  }

  it should "work with literals" in {
    [times: 5 ^1^^].e shouldBe Success((0 to 4).map(x => 1).toSeq)
  }

  it should "work with tuples" in {
    [times: 5 ^(1, 2)^^].e shouldBe Success((0 to 4).map(x => (1, 2)).toSeq)
  }

  it should "work with vars" in {
    [
      var x: Int = 3
      ^x^^
    ].e shouldBe Success(Seq(3))
  }

  it should "populate unsuccessful passes with nulls" in {
    ([@{there.pass = 2}: ^1^^]).e                         shouldBe Success(Seq(null, null, 1      ))
    ([@{there.pass = 1}: ^1^^ @{there.pass = 3}: ^2^^]).e shouldBe Success(Seq(null, 1   , null, 2))
  }

  "Double caret with numbers" should "work with code blocks" in {
    [{!1!}^^1 {!2!}^^2].e shouldBe Success((1, 2))
  }

  it should "work with script calls" in {
    [n1^^2 n2^^1].e shouldBe Success((2, 1))
  }

  it should "work with literals" in {
    [^1^^3 ^2^^1].e shouldBe Success((2, null, 1))
  }

  it should "work for tuples" in {
    [^(1, 2)^^1 ^(2, 3)^^2].e shouldBe Success( ((1, 2), (2, 3)) )
  }

  it should "work with vars" in {
    [
      var x: Int = 3
      ^x^^1 ^x^^2
    ].e shouldBe Success((3, 3))
  }

  "Dataflow map" should "work with pattern matches" in {
    [n1 ~~(r: Int)~~^ r * 2].e shouldBe(Success(2))
  }

  it should "work with multiple patterns" in {
    [
      n2 ~~(r: Int if r <  0)~~^ -r * 2
       +~~(r: Int if r >= 0)~~^  r * 2
    ].e shouldBe(Success(4))
  }

  it should "work in a shortened version" in {
    def triple(x: Int) = x * 3
    ([n2 ~~^ triple]).e shouldBe Success(6)
  }

  "Implicit caret" should "work in case of a script containing one argument of type T_code_fragment or T_call" in {
    def foo: Unit = ()
    ([@foo: n1]).e shouldBe Success(1)
  }

}

trait SubScriptSuiteHelpers {
  script..
    n1 = {!1!}
    n2 = {!2!}
}