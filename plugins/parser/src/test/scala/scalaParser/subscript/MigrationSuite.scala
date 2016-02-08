package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.Ast

object MigrationSuite extends TestSuite with Checkers with Symbols {def tests = TestSuite {'subscript {
  
  // Braces
  * - checkExpr9("a [b | c]", s"$seq($a, $parOr($b, $c))")

  // Script calls
  * - checkExpr9("a: b, c" , s"""$scriptCall a($varCall("b"), $varCall("c")))""")
  * - checkExpr9("a: 1"    , s"""$scriptCall a($varCall("1")))"""               )
  * - checkExpr9("a: b c d", s"""$seq($scriptCall a($varCall("b"))), $c, $d)""" )

  * - checkExpr9(
    "while x"
  , s"""subscript.DSL._while (_node => {
      |  implicit val here = _node
      |$varCall("x")
      |})""".stripMargin
  )
  * - checkExpr9(
    "while (!x)"
  , s"""subscript.DSL._while (_node => {
      |  implicit val here = _node
      |$varCall("($varCall(\\"!x\\"))")
      |})""".stripMargin
  )
  * - checkExpr9(
    "while (!x)"
  , s"""subscript.DSL._while (_node => {
      |  implicit val here = _node
      |$varCall("!x")
      |})""".stripMargin
  )

  // Scala code in scripts
  * - checkExpr9("a (b) c"   , s"""$seq($a, $scriptCall $varCall("b")), $c)""")
  * - checkExpr9("a (b, c) d", s"""$seq($a, $scriptCall ($varCall("b"), $varCall("c"))), $d)""")

  // Code blocks
  * - checkExpr9(
    "{:a:}"
    , s"""$tiny[Any] ($nodeVal => {
         |  implicit val $hereVal = $nodeVal
         |subscript.DSL._maybeVarCall("a")
         |}, true)""".stripMargin
  )
  * - checkExpr9(
    "{!a!}"
  , s"""$normal[Any] ($nodeVal => {
       |  implicit val $hereVal = $nodeVal
       |subscript.DSL._maybeVarCall("a")
       |}, true)""".stripMargin
  )

  // Let statements
  * - checkExpr9(
    "let a = b"
  , s"""$tiny[Any] ($nodeVal => {
       |  implicit val $hereVal = $nodeVal
       |$varCall("$varAss(\\"a\\", $varCall(\\"b\\"))")
       |}, true)""".stripMargin
  )

  * - checkExpr9(
    """a
      |let b
      |c""".stripMargin
  , s"""$seq($a, $tiny[Any] ($nodeVal => {
       |  implicit val $hereVal = $nodeVal
       |$varCall("b")
       |}, true), $c)""".stripMargin
  )

  * - checkExpr9(
    "a let b; c"
  , s"""$seq($a, $tiny[Any] ($nodeVal => {
     |  implicit val $hereVal = $nodeVal
     |$varCall("b")
     |}, true), $c)""".stripMargin
  )

  * - checkExpr9(
    "a let {b; c}; d"
  , s"""$seq($a, $tiny[Any] ($nodeVal => {
     |  implicit val $hereVal = $nodeVal
     |$varCall("{$varCall(\\"b\\"); $varCall(\\"c\\")}")
     |}, true), $d)""".stripMargin
  )

  * - checkExpr9(
    "a let b c d; d"
  , s"""$seq($a, $tiny[Any] ($nodeVal => {
     |  implicit val $hereVal = $nodeVal
     |$varCall("b c d")
     |}, true), $d)""".stripMargin
  )


  // Shorthand operators - ???
  // * - checkExpr9(
  //   """+ if speed>minSpeed then speedDecButton
  //     |  if speed<maxSpeed then speedIncButton""".stripMargin
  // , s"""$alt(${iff(s"""varCall("speed>minSpeed")""")}($scriptCall speedDecButton)), ${iff(s"""$varCall("speed<maxSpeed")""")}($scriptCall speedIncButton)))"""
  // )

}}}