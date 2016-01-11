package scalaParser.subscript

import scalaParser.subscript.ast.Constants._
import scalaParser.subscript.ast.Constants.DSL._

trait Symbols {
  def dsl(method: String) = s"subscript.DSL.$method"
  def call(sym: String  ) = s"""subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) => ${varc('"' + sym + '"')})"""
  def varc(sym: String)    = s"""subscript.DSL._maybeVarCall($sym)"""
  def script(name: String) = s"""subscript.DSL._script[Any](None, Symbol("$name")){(_node: subscript.vm.Script[Any]) =>
                                |  implicit val script = _node""".stripMargin

  def script(name: String, body: String) =
    s"""subscript.DSL._script[Any](None, Symbol("$name")){(_node: subscript.vm.Script[Any]) =>
       |  implicit val script = _node
       |$body}""".stripMargin

  def lambda(body: String) = script("<lambda>", body)

  def ifBase(method: String, condition: String) =      
    s"""$method (_node => {
       |  implicit val here = _node
       |${condition}
       |})""".stripMargin
  def iff    (condition: String) = ifBase(ifOp  , condition)
  def iffElse(condition: String) = ifBase(ifElse, condition)

  def annotation(body: String, target: String, nType: String = Type.CALL_GRAPH_NODE, tType: String = Type.TEMPLATE_CHILD) =
    s"""${Term.ANNOTATION}[$nType, $tType](${Name.HERE} => {
   |  implicit val ${Name.THERE}: $nType = here.there;
   |$body
   |}).apply($target)""".stripMargin



  val seq        = dsl("_seq")
  val ifOp       = dsl("_if")
  val ifElse     = dsl("_if_else")
  val doThen     = dsl("_do_then")
  val doThenElse = dsl("_do_then_else")
  val doElse     = dsl("_do_else")
  val parOr      = dsl("_par_or" )
  val parOr2     = dsl("_par_or2")
  val par        = dsl("_par")
  val parAnd2    = dsl("_par_and2")
  val parEqual   = dsl("_par_equal")
  val alt        = dsl("_alt")

  val disrupt          = dsl("_disrupt")
  val shuffle          = dsl("_shuffle")
  val shuffle1OrMore   = dsl("_shuffle_1_or_more")
  val interrupt        = dsl("_interrupt")
  val interrupt0OrMore = dsl("_interrupt_0_or_more")

  val normal0             = dsl("_normal0")
  val threaded0           = dsl("_threaded0")
  val unsure0             = dsl("_unsure0")
  val tiny0               = dsl("_tiny0")
  val eventhandling0      = dsl("_eventhandling0")
  val eventhandling_loop0 = dsl("_eventhandling_loop0")

  val normal             = dsl("_normal")
  val threaded           = dsl("_threaded")
  val unsure             = dsl("_unsure")
  val tiny               = dsl("_tiny")
  val eventhandling      = dsl("_eventhandling")
  val eventhandling_loop = dsl("_eventhandling_loop")

  val a = call("a")
  val b = call("b")
  val c = call("c")
  val d = call("d")
  val e = call("e")
  val f = call("f")

  val launchAnchor = dsl("_launch_anchor")
  val launch       = dsl("_launch"       )

  val scriptCall   = s"""subscript.DSL._maybeCall("", (here: subscript.vm.model.callgraph.CallGraphTreeNode) =>"""
  val varCall = "subscript.DSL._maybeVarCall"
  val varAss  = "subscript.DSL._maybeVarAssignment"
  val actorCall = s"$scriptCall r$$"

  val nodeVal   = "_node"
  val hereVal   = "here"
  val scriptVal = "`script`"

  val dfThen     = dsl("_dataflow_then")
  val dfThenElse = dsl("_dataflow_then_else")

  val loop    = dsl("_loop")
  val optLoop = dsl("_optionalBreak_loop")

}