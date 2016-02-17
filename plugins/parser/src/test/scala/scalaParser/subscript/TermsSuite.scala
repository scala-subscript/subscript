package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.Ast

object TermsSuite extends TestSuite with Checkers with Symbols {def tests = TestSuite {'subscript {
  
  * - checkBlocks(
    ""    -> normal
  , "*"   -> threaded
  , "?"   -> unsure
  , "!"   -> tiny
  , "."   -> eventhandling
  , "..." -> eventhandling_loop
  )

  * - checkRuleMulti(_.Expr1)(
    "var x: Int = 3" -> s"""subscript.DSL._var(x, (_node: subscript.vm.N_localvar[Int]) => {implicit val here = _node; val tr: Int = $varCall("3"); tr})"""
  , "val x: Int = 3" -> s"""subscript.DSL._val(x, (_node: subscript.vm.N_localvar[Int]) => {implicit val here = _node; val tr: Int = $varCall("3"); tr})"""
  
  , "val x: Int = y" -> s"""subscript.DSL._val(x, (_node: subscript.vm.N_localvar[Int]) => {implicit val here = _node; val tr: Int = $varCall("y"); tr})"""
  )

  * - checkRuleStrMulti(_.StatCtx.Expr)(
    "[a]"   ->
    s"""subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
       |  implicit val script = _node
       |$a}""".stripMargin
  
  , "[ a ]" ->
    s"""subscript.DSL._script[Any](None, Symbol("lambda")){(_node: subscript.vm.Script[Any]) =>
       |  implicit val script = _node
       |$a}""".stripMargin
  )

  * - checkExpr1("a"            , s"""$scriptCall a)"""                                                                       )
  * - checkExpr1("a(x)"         , s"""$scriptCall a($varCall("x")))"""                                                          )
  * - checkExpr1("a.b.c(x)"     , s"""$scriptCall a.b.c($varCall("x")))"""                                                      )
  * - checkExpr1("a(a, b, c)"   , s"""$scriptCall a($varCall("a"), $varCall("b"), $varCall("c")))"""                                )
  * - checkExpr1("a(?x)"        , s"""subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a(subscript.DSL._maybeVarCall("subscript.DSL._actualOutputParameter(\\"x\\")")))""")
  * - checkExpr1("a(??x)"       , s"""subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a(subscript.DSL._maybeVarCall("subscript.vm.ActualAdaptingParameter(x)")))"""                              )
  * - checkExpr1("a(new A)"     , s"""$scriptCall a($varCall("new A")))"""                                                                )
  * - checkExpr1("a(f[Int](x))" , s"""subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a(subscript.DSL._maybeVarCall("f[Int](subscript.DSL._maybeVarCall(\\"x\\"))")))"""                                                  )
  * - checkExpr1("a(f(x))"      , s"""subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => a(subscript.DSL._maybeVarCall("f(subscript.DSL._maybeVarCall(\\"x\\"))")))"""                                                       )
  * - checkExpr1("a(a.b.c)"     , s"""$scriptCall a($varCall("a.b.c")))"""                                                      )
  * - checkExpr1("a(a.b.c(x))"  , s"""$scriptCall a($varCall("a.b.c($varCall(\\"x\\"))")))"""                                 )

  * - checkExpr1("true", s"$scriptCall true)")
  * - checkExpr1("1"   , s"$scriptCall 1)"   )
  * - checkExpr1("'c'" , s"$scriptCall 'c')" )

  * - checkExpr1(
    "<< A => B >>"

  , s"""$actorCall({
       |case A => $varCall("B")
       |null
       |}))""".stripMargin
  )

  * - checkExpr1(
    s"""<<
       |  case A => b
       |  case C => d
       |>>""".stripMargin

  , s"""$actorCall({
       |case A => $varCall("b")
       |
       |null
       |case C => $varCall("d")
       |
       |null
       |}))""".stripMargin
  )

  * - checkExpr1(
    "<< A => B ==> c >>"

  , s"""$actorCall({
       |case A => $varCall("B")
       |${lambda(c)}
       |}))""".stripMargin
  )

  * - checkExpr1(
    s"""<<
       |  case A => b
       |  ==>
       |    a
       |  case C => d
       |>>""".stripMargin

  , s"""$actorCall({
       |case A => $varCall("b")
       |
       |${lambda(a)}
       |case C => $varCall("d")
       |
       |null
       |}))""".stripMargin
  )

  * - checkExpr1(
    "<<A>>"

  , s"""$actorCall({
       |case A =>
       |null
       |}))""".stripMargin
  )

  * - checkExpr1(
    """<<
      |  case A
      |  case B
      |>>""".stripMargin

  , s"""$actorCall({
       |case A =>
       |null
       |case B =>
       |null
       |}))""".stripMargin
  )

  * - checkExpr1(
    "<<A ==> b>>"

  , s"""$actorCall({
       |case A =>
       |${lambda(b)}
       |}))""".stripMargin
  )

  * - checkExpr9(
    s"""a b <<case A => b ==> c>> d"""

  , s"""$seq($a, $b, $actorCall({
       |case A => $varCall("b")
       |${lambda(c)}
       |})), $d)""".stripMargin
  )

  
  * - checkExpr9("event(          AnyEventReactor[Any,N_code_eventhandling[Any]](comp))"            , s"""$scriptCall event(          $varCall("AnyEventReactor[Any,N_code_eventhandling[Any]]($varCall(\\"comp\\"))")))""")
  * - checkExpr9("event(     WindowClosingReactor[Any,N_code_eventhandling[Any]](window))"          , s"""$scriptCall event(     $varCall("WindowClosingReactor[Any,N_code_eventhandling[Any]]($varCall(\\"window\\"))")))""")
  * - checkExpr9("event_loop( MousePressedReactor[Any,N_code_eventhandling_loop[Any]](comp), task)" , s"""$scriptCall event_loop( $varCall("MousePressedReactor[Any,N_code_eventhandling_loop[Any]]($varCall(\\"comp\\"))"), $varCall("task")))""")


  def checkNormalInScript(in: String, out: String) =
    checkRuleStr(p => p.WithNormalInScript(() => p.StatCtx.Expr), in, out)
  
  * - checkNormalInScript("a"            , s"""$varCall("a")""")
  * - checkNormalInScript("a(x)"         , s"""$varCall("a($varCall(\\"x\\"))")""")
  * - checkNormalInScript("a.b.c"        , s"""$varCall("a.b.c")""")
  * - checkNormalInScript("a.b.c(x)"     , s"""$varCall("a.b.c($varCall(\\"x\\"))")""")
  * - checkNormalInScript("new A"        , s"""$varCall("new A")""")
  * - checkNormalInScript("f[Int](x)"    , s"""$varCall("f[Int]($varCall(\\"x\\"))")""")
  * - checkNormalInScript("a(x).apply(y)", s"""$varCall("a($varCall(\\"x\\")).apply($varCall(\\"y\\"))")""")
  * - checkNormalInScript("a b c"        , s"""$varCall("a b c")""")
  * - checkNormalInScript("a + c"        , s"""$varCall("a + c")""")
  * - checkNormalInScript("a(y).m.n"     , s"""$varCall("a($varCall(\\"y\\")).m.n")""")
  * - checkNormalInScript("a.apply(b)"   , s"""$varCall("a.apply($varCall(\\"b\\"))")""")
  * - checkNormalInScript("a(x).b.c"     , s"""$varCall("a($varCall(\\"x\\")).b.c")""")

  * - checkNormalInScript("a = b"              , s"""$varCall("$varAss(\\"a\\", $varCall(\\"b\\"))")""")
  * - checkNormalInScript("a = 3"              , s"""$varCall("$varAss(\\"a\\", $varCall(\\"3\\"))")""")
  * - checkNormalInScript("a(x).b.c = a(y).m.n", s"""$varCall("$varAss(\\"a($varCall(\\\\\\"x\\\\\\")).b.c\\", $varCall(\\"a($varCall(\\\\\\"y\\\\\\")).m.n\\"))")""")
  
  * - checkNormalInScript("""println(s"x")"""  , s"""$varCall("println($varCall(\\"s\\\\\\\"x\\\\\\\"\\"))")""")


  * - checkRuleMulti(_.Expr1)(
    "while(x)" ->
    """subscript.DSL._while (_node => {
      |  implicit val here = _node
      |x
      |})""".stripMargin

  , "[-]"   -> "subscript.DSL._deadlock"
  , "[+]"   -> "subscript.DSL._empty"
  , "[+-]"  -> "subscript.DSL._neutral"
  , "."     -> "subscript.DSL._optionalBreak"
  , ".."    -> "subscript.DSL._optionalBreak_loop"
  , "..."   -> "subscript.DSL._loop"
  , "break" -> "subscript.DSL._break"
  )


  // Nice script call syntax
  * - checkExpr9("a,b,c"  , s"""$scriptCall a($varCall("b"), $varCall("c")))""")
  * - checkExpr9("a,1"    , s"""$scriptCall a($varCall("1")))"""                       )
  * - checkExpr9("a,b c d", s"""$seq($scriptCall a($varCall("b"))), $c, $d)""")

  * - checkExpr9(
    """a,b
      |c""".stripMargin

  , s"""$seq($scriptCall a($varCall("b"))), $c)""".stripMargin
  )

  * - checkExpr9(
    "a (b,c,(2 + 3),d(a, b),e) c"

  , s"""$seq($a, $scriptCall b($varCall("c"), $varCall("($varCall(\\"2 + 3\\"))"), $varCall("d($varCall(\\"a\\"), $varCall(\\"b\\"))"), $varCall("e"))), $c)"""
  )

  * - checkExpr9(
    """a,b
      |a c,d e""".stripMargin

  , s"""$seq($scriptCall a($varCall("b"))), $seq($a, $scriptCall c($varCall("d"))), $e))"""
  )


  // Bugs
  // #49
  def checkCodeBlock(in: String, out: String) = checkExpr1(
    s"{$in}"
  , s"""$normal[Any] ($nodeVal => {
   |  implicit val $hereVal = $nodeVal
   |$out
   |}, true)""".stripMargin
  )

  * - checkCodeBlock(
    "val a = b"
  , s"""val a = $varCall("b")"""
  )
  * - checkCodeBlock(
    "var a = b"
  , s"""var a = $varCall("b")"""
  )
  * - checkCodeBlock(
    "def a(x: Int, y: Int) = b"
  , s"""def a(x: Int, y: Int) = $varCall("b")"""
  )

  // #48
  * - checkExpr9(
    "@gui: a"
  , annotation("gui", a)
  )

  * - checkExpr9(
    s"""@gui:
       |a""".stripMargin
  
  , annotation("gui", a)
  )

  // #46
  * - checkExpr9(
    "@gui(there): a"
  , annotation(s"""gui($varCall("there"))""", a)
  )
  * - checkExpr9(
    "@gui(there, somewhere): a"
  , annotation(s"""gui($varCall("there"), $varCall("somewhere"))""", a)
  )

}}}
