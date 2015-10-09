package scalaParser.subscript
package generic

import scala.util._

import utest._

import scalaParser.subscript.ast.Ast._
import scalaParser.subscript.util._

object CommunicationStackSuite extends TestSuite
                                  with Checkers
                                  with CommunicationStackSuiteHelpers {
  import CSSHImplicits._

  def tests = TestSuite {
    'communicationStack {

      // push
      * - stackFull(1) {s =>
        s.stack.size shouldBe 1
        s.stack.head shouldBe map(1)
      }

      // FIFO
      * - stackFull(2) {s =>
        s.stack.size shouldBe 2
        s.stack(0) shouldBe map(2)
        s.stack(1) shouldBe map(1)
      }

      // last
      * - stackFull(2) {s =>
        s.last(2) shouldBe map(1).merge(map(2))
      }

      // reduce
      * - stackFull(2) {s =>
        s.reduce(2)
        s.stack.size shouldBe 1
        s.stack.head shouldBe map(1).merge(map(2))
      }

      // merge test
      * - stack {s =>
        s push Map("a" -> 1, "b" -> 2)
        s push Map("a" -> 1, "c" -> 3)
        s push Map("a" -> 3, "b" -> 4)

        s.last(3) shouldBe Map("a" -> List(1, 1, 3), "b" -> List(2, 4), "c" -> List(3))
      }

    }
  }

}

trait CommunicationStackSuiteHelpers {

  def newStack: CommunicationStack = new CommunicationStackImpl

  def stack(f: CommunicationStack => Unit): Unit = f(newStack)

  def stackFull(size: Int)(f: CommunicationStack => Unit): Unit = stack {s =>
    for (n <- 1 to size) s push map(n)
    f(s)
  }

  def map(x: Int): Map[String, Any] = Map(s"SAMPLE$x" -> x)


  object CSSHImplicits {
    implicit class MapRefinements[K, V](m: Map[K, V]) {
      def merge(another: Map[K, V]): Map[K, List[V]] = (m.toSeq ++ another.toSeq)
        .groupBy(_._1)
        .mapValues {_.map(_._2).toList}

    }

    implicit class InfixMatcher(x: Any) {
      def shouldBe(y: Any) =
        if (x != y) throw new RuntimeException(s"${x.toString} is not ${y.toString}")
    }
  }

}