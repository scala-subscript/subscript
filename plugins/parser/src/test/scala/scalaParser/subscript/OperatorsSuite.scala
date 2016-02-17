package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.Ast

object OperatorsSuite extends TestSuite with Checkers with Symbols {def tests = TestSuite {'subscript {

  * - checkExpr9("a"            , s"$a")
  * - checkExpr9("a b"          , s"$seq($a, $b)")
  * - checkExpr9("a;b c"        , s"$seq($a, $seq($b, $c))")
  * - checkExpr9("a . b"        , s"$seq($a, subscript.DSL._optionalBreak, $b)")
  * - checkExpr9("a ; b c ; d e", s"$seq($a, $seq($b, $c), $seq($d, $e))")

  * - checkExpr9("if a then b"               , s"${iff(s"""$varCall("a")""")}($b)")
  * - checkExpr9("if a then b else c"        , s"${iffElse(s"""$varCall("a")""")}($b, $c)")
  * - checkExpr9("if  true then a   else c"  , s"${iffElse(s"""$varCall("true")""")}($a, $c)")
  * - checkExpr9("if false then a b else c d", s"${iffElse(s"""$varCall("false")""")}($seq($a, $b), $seq($c, $d))")
  
  * - checkExpr9("do a then b"               , s"$doThen($a, $b)")
  * - checkExpr9("do a then b else c"        , s"$doThenElse($a, $b, $c)")
  * - checkExpr9("do a else c"               , s"$doElse($a, $c)")
  * - checkExpr9("do a     then b c else d e", s"$doThenElse($a, $seq($b, $c), $seq($d, $e))")
  * - checkExpr9("do   [+] then b"           , s"$doThen(subscript.DSL._empty, $b)")
  * - checkExpr9("do a [-] then b c else d e", s"$doThenElse($seq($a, subscript.DSL._deadlock), $seq($b, $c), $seq($d, $e))")

  * - checkExpr9("a  b | c"     , s"$parOr($seq($a, $b), $c)")
  * - checkExpr9("a (b | c)"    , s"$seq($a, $parOr($b, $c))")

  * - checkExpr9("(* *)"        , s"$launch()")
  * - checkExpr9("(* a b *)"    , s"$launch($seq($a, $b))")
  * - checkExpr9("(** a b **) c", s"$seq($launchAnchor($seq($a, $b)), $c)")
  * - checkExpr9("(*  a b  *) c", s"$seq($launch($seq($a, $b)), $c)")

  * - checkExpr9("(* a (b c *) *)"      , s"$launch($seq($a, $seq($b, $c, ${call("*")})))")
  * - checkExpr9("(** a * ** **)"       , s"$launchAnchor($seq($a, ${call("*")}, ${call("**")}))")
  * - checkExpr9("(** a (* b c *) d **)", s"$launchAnchor($seq($a, $launch($seq($b, $c)), $d))")
  
  * - checkExpr9(
    """a
      |b""".stripMargin
  
  , s"$seq($a, $b)")

  // "a" has an extra space after it!
  * - checkExpr9(
    """a 
      |b""".stripMargin
    
  , s"$seq($a, $b)"
  )


  * - checkExpr9(
    "|| a b c"
  , s"$parOr2($a, $b, $c)"
  )

  * - checkExpr9(
    """||| a b
      |   c d""".stripMargin
  
  , s"$parOr2($a, $b, $c, $d)"
  )

  * - checkExpr9(
    """||| a b
      |c d""".stripMargin
  
  , s"$parOr2($a, $b, $c, $d)"
  )

  * - checkExpr9(
    """| || a b
      |c d""".stripMargin
  
  , s"$parOr2($a, $b)"
  )

  * - checkExpr9(
    "a b (+ c d) e f"
  , s"$seq($a, $b, $alt($c, $d), $e, $f)"
  )


  * -> checkOps(
    "|"    -> parOr
  , "||"   -> parOr2
  , "&"    -> par
  , "&&"   -> parAnd2
  , "=="   -> parEqual
  , "+"    -> alt
  , "/"    -> disrupt
  , "%"    -> shuffle
  , "%%"   -> shuffle1OrMore
  , "%/"   -> interrupt
  , "%/%/" -> interrupt0OrMore
  )

  // Dataflow
  * - checkExpr9(
    s"""a ~~(x: Boolean)~~> b"""

  , s"""$dfThen(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: Boolean => ${lambda(b)}}
       |)""".stripMargin
  )
  * - checkExpr9(
    s"""a ~~(x: String)~~> b +~/~(e: RuntimeException) ~~> c"""

  , s"""$dfThenElse(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: String => ${lambda(b)}}
       |, (_e: Any) => _e match {case e: RuntimeException => ${lambda(c)}}
       |)""".stripMargin
  )

  * - checkExpr9(
    s"""a ~~(x: String   )~~> b
       |+~/~(t: Throwable)~~> c""".stripMargin

  , s"""$dfThenElse(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: String => ${lambda(b)}}
       |, (_t: Any) => _t match {case t: Throwable => ${lambda(c)}}
       |)""".stripMargin
  )

  * - checkExpr9(
    s"""a
       |~~(x: String)~~>
       |b
       |+~/~(t: Throwable)~~>
       |c""".stripMargin

  , s"""$dfThenElse(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: String => ${lambda(b)}}
       |, (_t: Any) => _t match {case t: Throwable => ${lambda(c)}}
       |)""".stripMargin
  )

  * - checkExpr9(
    s"""e
       |a
       |~~(x: String)~~>
       |b
       |+~/~(t: Throwable)~~>
       |c
       |f""".stripMargin

  , s"""$seq($e, $dfThenElse(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: String => ${lambda(b)}}
       |, (_t: Any) => _t match {case t: Throwable => ${lambda(c)}}
       |), $f)""".stripMargin
  )

  * - checkExpr9(
    s"""a
       |~~(x: String)~~>
       |b""".stripMargin

  , s"""$dfThen(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: String => ${lambda(b)}}
       |)""".stripMargin
  )

  * - checkExpr9(
    s"""a b ~~(x: String)~~> c d"""

  , s"""$seq($a, $dfThen(
       |  ${script("~~>", b)}
       |, (_x: Any) => _x match {case x: String => ${lambda(c)}}
       |), $d)""".stripMargin
  )

  * - checkExpr9(
    s"""(a b) ~~(x: String)~~> (c d) +~/~(t: Throwable)~~> (e f)"""

  , s"""$dfThenElse(
       |  ${script("~~>", s"$seq($a, $b)")}
       |, (_x: Any) => _x match {case x: String => ${lambda(s"$seq($c, $d)")}}
       |, (_t: Any) => _t match {case t: Throwable => ${lambda(s"$seq($e, $f)")}}
       |)""".stripMargin
  )

  * - checkExpr9(
    s"""a ~~ (x: Boolean) ~~> b"""

  , s"""$dfThen(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: Boolean => ${lambda(b)}}
       |)""".stripMargin
  )

  * - checkExpr9(
    s"""a ~~ ( x :String   ) ~~> b
       |+~/~ (   t  :Throwable  ) ~~> c""".stripMargin

  , s"""$dfThenElse(
       |  ${script("~~>", a)}
       |, (_x: Any) => _x match {case x: String => ${lambda(b)}}
       |, (_t: Any) => _t match {case t: Throwable => ${lambda(c)}}
       |)""".stripMargin
  )


  // #33
  * - checkExpr9(
    "...a"
  , s"""$seq($loop, $a)"""
  )
  * - checkExpr9(
    "a..."
  , s"$seq($a, $loop)"
  )
  * - checkExpr9(
    "..a"
  , s"$seq($optLoop, $a)"
  )
  * - checkExpr9(
    "a.."
  , s"$seq($a, $optLoop)"
  )
  * - checkExpr9(
    "a.b"
  , s"$scriptCall a.b)"
  )

  // #44
  * - checkExpr9(
    "if a then b ; c"
  , s"""$seq(${iff(s"""$varCall("a")""")}($b), $c)"""
  )
  * - checkExpr9(
    "if a then b & c else d & e; f"
  , s"""$seq(${iffElse(s"""$varCall("a")""")}($par($b, $c), $par($d, $e)), $f)"""
  )
  * - checkExpr9(
    """if a then b
      |          c""".stripMargin

  , s"""$seq(${iff(s"""$varCall("a")""")}($b), $c)"""
  )
  * - checkExpr9(
    "do a then b else c; d"
  , s"""$seq($doThenElse($a, $b, $c), $d)"""
  )
  * - checkExpr9(
    """if a then (
      |  a b
      |  c d
      |)
      |else e""".stripMargin
  
  , s"""${iffElse(s"""$varCall("a")""")}($seq($seq($a, $b), $seq($c, $d)), $e)"""
  )

}}}
