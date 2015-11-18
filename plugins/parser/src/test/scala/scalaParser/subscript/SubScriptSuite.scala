package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.Ast

object SubScriptSuite extends TestSuite with Checkers with Symbols {
  
  def tests = TestSuite {
    'subscript {
      * - check(
        "script a",
        "def a"
      )

      * - check(
        "script a(b: Int = 2, y: String, ?z: Foo)(c: String)(implicit m: X): Int",
        "def a(b: Int = 2, y: String, z: subscript.vm.FormalOutputParameter[Foo])(c: String)(implicit m: X): subscript.vm.ScriptNode[Int]"
      )

      * - check(
        "script a = a ; b ; c"
      , s"""def a = subscript.DSL._script[Any](None, Symbol("a")){(_node: subscript.vm.Script[Any]) =>
           |  implicit val script = _node
           |$seq($a, $b, $c)}""".stripMargin
      )

      * - check(
        "script a(??c: Foo)"
      , """def a(c: subscript.vm.FormalConstrainedParameter[Foo])"""
      )

      * - check(
        "script a(??c: Foo) = a"
      , s"""def a(c: subscript.vm.FormalConstrainedParameter[Foo]) = subscript.DSL._script[Any](None, Symbol("a"), c.~??(Symbol("c"))){(_node: subscript.vm.Script[Any]) =>
          |  implicit val script = _node
          |$a}""".stripMargin
      )

      * - check(
        "script a(?c: Foo) = a"
      , s"""def a(c: subscript.vm.FormalOutputParameter[Foo]) = subscript.DSL._script[Any](None, Symbol("a"), c.~?(Symbol("c"))){(_node: subscript.vm.Script[Any]) =>
          |  implicit val script = _node
          |$a}""".stripMargin
      )
      
      * - check(
        """script..
          |  a = a
          |  b = b""".stripMargin
      , s"""def a = subscript.DSL._script[Any](None, Symbol("a")){(_node: subscript.vm.Script[Any]) =>
           |  implicit val script = _node
           |$a}
           |def b = subscript.DSL._script[Any](None, Symbol("b")){(_node: subscript.vm.Script[Any]) =>
           |  implicit val script = _node
           |$b}""".stripMargin
      )

      * - check(
        """script..
          |  a =    a
          |      b
          |     c
          |    d""".stripMargin
      , s"""def a = subscript.DSL._script[Any](None, Symbol("a")){(_node: subscript.vm.Script[Any]) =>
           |  implicit val script = _node
           |$seq($a, $b, $c)}
           |def d""".stripMargin
      )

      * - check(
        """script..
          |  a =
          |    a
          |     b
          |   c
          |  d""".stripMargin
      , s"""def a = subscript.DSL._script[Any](None, Symbol("a")){(_node: subscript.vm.Script[Any]) =>
           |  implicit val script = _node
           |$seq($a, $b)}
           |def c
           |def d""".stripMargin
      )


      * - checkRuleStr(_.TmplBody,
        """{
          |  script..
          |    a = a
          |  b = b
          |  script c = c
          |}""".stripMargin
      , s"""{
           |def a = ${script("a")}
           |$a}
           |def b = ${script("b")}
           |$b}
           |def c = ${script("c")}
           |$c}
           |}""".stripMargin
      )

      * - checkRuleStr(_.TmplBody,
        """{implicit script a}"""
      , "{implicit def a}"
      )

      * - checkRuleStr(_.TmplBody,
        """{
          |implicit script..
          |  a
          |  b
          |}""".stripMargin
      , """{
          |implicit def a
          |implicit def b
          |}""".stripMargin
      )

      * - checkRuleStr(_.TmplBody,
        """{
          |override script..
          |  a = a
          |  b = b
          |}""".stripMargin
      , s"""{
           |override def a = ${script("a")}
           |$a}
           |override def b = ${script("b")}
           |$b}
           |}""".stripMargin
      )

      // #47
      * - checkRuleStr(_.TmplBody,
        "{private script a}"
      , "{private def a}"
      )
      * - checkRuleStr(_.TmplBody,
        "{private implicit script a}"
      , "{private implicit def a}"
      )
      * - checkRuleStr(_.TmplBody,
        "{protected[this] script a}"
      , "{protected[this] def a}"
      )
      * - checkRuleStr(_.TmplBody,
        """{
          |private[this] script..
          |    a
          |    b
          |}""".stripMargin
      
      , """{
          |private[this] def a
          |private[this] def b
          |}""".stripMargin
      )

    }
  }

}