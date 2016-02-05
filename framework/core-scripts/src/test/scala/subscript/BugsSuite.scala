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

}