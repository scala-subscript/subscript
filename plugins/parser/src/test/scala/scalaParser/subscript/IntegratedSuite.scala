package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.Ast

object IntegratedSuite extends TestSuite with Checkers with Symbols {def tests = TestSuite {'subscript {

  * - checkFile("core/OperatorsSuite")
  * - checkFile("core/Test")

  * - checkFile("swing/Scripts")
  * - checkFile("swing/SubScriptDebugger")

  * - checkFile("examples/helloworld-example/HelloWorld")
  * - checkFile("examples/helloworld-example/Hello_1_World")

  * - checkFile("examples/ab-example/AB")
  * - checkFile("examples/ab-example/Bag")
  * - checkFile("examples/ab-example/Bag_AB")

  * - checkFile("examples/life-example/BasicLifeFrame")
  * - checkFile("examples/life-example/LifeFrame")

  * - checkFile("examples/lookupframe-example/LookupFrame")
  * - checkFile("examples/lookupframe-example/LookupFrame2")

  * - checkFile("examples/subscript-twitter-search/SubScriptController")
  * - checkFile("examples/subscript-twitter-search/SubScriptFuturesController")

  * - checkFile("examples/taskprocessor/FrontProcessor")
  * - checkFile("examples/taskprocessor/Processor")
  * - checkFile("examples/taskprocessor/Proxy")

}}}
