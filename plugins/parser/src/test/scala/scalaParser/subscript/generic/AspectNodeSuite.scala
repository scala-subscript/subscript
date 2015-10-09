package scalaParser.subscript
package generic

import scala.util._

import utest._

import scalaParser.subscript.ast.Ast._

object AspectNodeSuite extends TestSuite with Checkers with Symbols {

  case class RootNode(c: ChildNode) extends Node with AspectNode {
    val name = "root"

    def rewriteRaw(implicit context: Context, output: Output): String =
      s"My child node is ${c.compile}"
  }

  class ChildNode extends Node {
    def rewrite(implicit context: Context, output: Output): String = {
      push("root_before" -> List("hello", "world") )
      push("root_after"  -> List("after", "aspect"))
      "Child Node"
    }
  }


  def checkStr(str1: String, str2: String): Unit =
    if (str1 != str2) s"""Mismatch.
                         |--- EXPECTED ---
                         |$str2
                         |--- OBSERVED ---
                         |$str1
                         |----------------
                      """.stripMargin

  def tests = TestSuite {
    'subscript {

      * - checkStr(
        RootNode(new ChildNode).compile
      , """{
          |  hello
          |  world
          |  My child node is Child Node
          |  after
          |  aspect
            }"""
      )

    }
  }
}
