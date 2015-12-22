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

}
