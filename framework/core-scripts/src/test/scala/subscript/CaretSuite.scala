package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

class CaretSuite extends FlatSpec with Matchers
                                  with CommonHelpers {

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

  it should "work in prefix position before complex var/method calls" in {
    object foo {
      class Bar(x: Int) {
        def getX = x
        def get(y: Int) = y
      }

      def bar(x: Int) = new Bar(x)
    }

    var i = 0

    script s =
      ^foo.bar(1).getX   ~~(x: Int)~~> let i += x
      ^foo.bar(1).get(2) ~~(x: Int)~~> let i += x
      ^foo.bar(1).get: 2 ~~(x: Int)~~> let i += x

    runScript(s)

    i shouldBe 5
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

  "Implicit caret" should "work in case of a script containing one argument of type T_code_fragment or T_call" in {
    def foo: Unit = ()
    ([@foo: n1]).e shouldBe Success(1)
  }

}
