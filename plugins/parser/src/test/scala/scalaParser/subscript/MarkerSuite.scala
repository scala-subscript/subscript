package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.{Ast, Constants}

object MarkerSuite extends TestSuite with Checkers with Symbols {def tests = TestSuite {'subscript {

  * - marker(false)(
    s"""package torimatomeru
      |
      |package lols
    """.stripMargin
  )
  * - marker(true)(
    s"""package torimatomeru
      |import a
      |import ${Constants.Name.FILE}
      |import b
    """.stripMargin
  )
  * - marker(false)(
    s"""
      |package torimatomeru
      |
      |import org.parboiled2.ParseError
      |import utest._
      |import utest.framework.Test
      |import utest.util.Tree
      |
      |import scala.util.{Failure, Success}
      |
      |object SyntaxTest extends TestSuite
    """.stripMargin
  )

  // This one has 2 spaces after the import
  * - marker(true)(
    s"""
      |import ${Constants.Name.FILE}  
      |object SyntaxTest extends TestSuite{
      |  def marker[T](input: String) = {
      |
      |  }
      |}
    """.stripMargin
  )
  * - marker(false)(
    s"""
      |object SyntaxTest{
      |  a()
      |  throw 1
      |}
    """.stripMargin
  )
  * - marker(false)(
    s"""
      |object SyntaxTest extends TestSuite{
      |  {
      |        println
      |        throw 1
      |  }
      |}
    """.stripMargin
  )
  * - marker(true)(
    s"""package scalatex
      |
      |
      |import org.parboiled2._
      |import torimatomeru.ScalaSyntax
      |
      |import scalatex.stages.{Trim, Parser, Ast}
      |import scalatex.stages.Ast.Block.{IfElse, For, Text}
      |import Ast.Chain.Args
      |
      |import ${Constants.Name.FILE}
      |
      |object ParserTests extends utest.TestSuite{
      |  import Ast._
      |  import utest._
      |  def marker[T](input: String, parse: Parser => scala.util.Try[T], expected: T) = {
      |    val parsed = parse(new Parser(input)).get
      |    assert(parsed == expected)
      |  }
      |  def tests = TestSuite{}
      |}
    """.stripMargin
  )
  * - marker(false)(
    s"""
      |object Moo{
      |  a
      |  .b
      |
      |  c
      |}
    """.stripMargin
  )
  * - marker(false)(
    s"""
      |object Moo{
      | filename
      |        .asInstanceOf[Literal]
      |10
      |}
    """.stripMargin
  )
  * - marker(true)(
    s"""
      |import ${Constants.Name.FILE}
      |object Cow{
      |  ().mkString
      |
      |  1
      |}
    """.stripMargin
  )
}}}
