package subscript

import scala.util._

import subscript.language
import subscript.Predef._

import org.scalatest._

class BugsSuite extends FlatSpec with Matchers
                                  with CommonHelpers {

  "#26" should "work" in {
    var i = 0
    script foo = {!i += 1!};
                 {!i += 1!}

    runScript(foo)
    
    i shouldBe 2
  }

  "#28" should "work" in {
    script s = [^1^^1 ^2^^2]^^
    runScript(s).$ shouldBe Success(List((1, 2)))
  }

  "#29" should "be possible to call nice while with prefixed simple expression" in {
    var i = 0
    script s = var flag = false
               [while: !flag {!i += 1!} {!if (i >= 3) flag = true!}]
  
    runScript(s)
    i shouldBe 3
  }

  // it should "be possible to use prefixed expressions in nice script calls" in {
  //   script..
  //     a(x: Boolean) = if x then ^1 else ^2
  //     b = a: !true

  //   runScript(b).$ shouldBe Success(2)
  // }


}