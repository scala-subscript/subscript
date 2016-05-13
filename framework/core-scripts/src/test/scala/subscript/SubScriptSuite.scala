package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

class SubScriptSuite extends FlatSpec with Matchers
                                      with CommonHelpers {

  "Constructor calls" should "be possible without parenthesis" in {
    class Foo extends subscript.objectalgebra.SSProcess {
      override script live = [+]
    }

    ([new Foo ^new Foo new Foo]).e shouldBe a [Success[_]]
  }

  "Variables" should "be defineable in terms of other variables" in {
    [
      var x = 1
      var y = x + 1
      ^y
    ].e shouldBe Success(2)
  }

  it should "be defineable via blocks containing other variables (issue #5)" in {
    [
      val a = {
                val x = 3
                x
              }
      ^a
    ].e shouldBe Success(3)
  }

  "Script calls" should "be extracted from local vars" in {
    var flag = false
    case class Foo(x: Int) {
      def bar: Unit = flag = true
    }

    ([
      var foo = Foo(3)
      foo.bar
    ]).e

    flag shouldBe true
  }

  "Output params" should "be usable from a script" in {
    implicit script param2script(?x: Int) = let x = 10

    script foo =
      var bar = 3
      ?bar
      ^bar

    foo.e shouldBe Success(10)
  }

  "Constrained params" should "work" in {
    implicit script param2script(??x: Int) = let x = 10

    script foo =
      var bar = 3
      ?bar ?if (bar > 0)
      ^bar

    foo.e shouldBe Success(10)    
  }
  
  "Do construct" should "work for normal code" in {
    ([
      var foo = 3
      do! foo += 1
      ^foo
    ]).e shouldBe Success(4)
  }

  it should "work for threaded code" in {
    ([
      var foo = 3
      do* foo += 1
      ^foo
    ]).e shouldBe Success(4)
  }

  "Call graph nodes' parameters" should "work" in {
    ([
      do! here.n_ary_op_ancestor.setProperty("FOO", 1)
      {!here.n_ary_op_ancestor.getProperty[String, Int]("FOO")!}^
    ]).e shouldBe Success(Some(1))
  }

}
