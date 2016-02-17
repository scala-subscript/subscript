package scalaParser

import org.parboiled2._

/**
 * Some rules may be different depending on the context.
 * For example, Id must be wrapped in the _maybeVarCall macro
 * if in Script block context, and must be left as is otherwise.
 * Switches are object that store information about the current context.
 * Based on the state of a certain switch, rules may decide what
 * fors they should take.
 */
trait Switch {
  val DEFAULT = 0
  var state = DEFAULT
}

/**
 * RuleBank stores all possible rule variations with the states
 * under which these variations take palce.
 */
abstract class RuleBank[+T](val switch: Switch) {

  implicit def int2rule(x: Int): () => Rule1[T] = () => ruleMap(x).apply()

  def default = switch.DEFAULT

  def ruleMap: Map[Int, () => Rule1[T]]

  def apply(): () => Rule1[T] = ruleMap.withDefault(_ => ruleMap(default))(switch.state)

}

trait Switches {this: Metarules =>

  object ScopeSwitch extends Switch {
    val NORMAL            = DEFAULT
    val SCRIPT            = 1
    val NORMAL_IN_SCRIPT  = 2

    val PARENTHESES       = 3
    val LAUNCH_ANCHOR     = 4
    val LAUNCH            = 5

    val ASSIGNMENT        = 6
    val PATTERN           = 7

    val NICE_SCRIPT_CALL  = 8
  }

  abstract class ScopeBank[+T] extends RuleBank[T](ScopeSwitch)

  def WithScope[T](rle: () => Rule1[T], scope: Int): Rule1[T] = {
    var previousState = -1
    def reset = ScopeSwitch.state = previousState
    
    Try(
      before   = {previousState = ScopeSwitch.state; ScopeSwitch.state = scope}
    , rle      = rle
    , after    = reset
    , rollback = reset
    )
  }

  import ScopeSwitch._
  def WithNormal        [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, NORMAL          )
  def WithScript        [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, SCRIPT          )
  def WithNormalInScript[T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, NORMAL_IN_SCRIPT)
  def WithParentheses   [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, PARENTHESES     )
  def WithLaunchAnchor  [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, LAUNCH_ANCHOR   )
  def WithLaunch        [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, LAUNCH          )
  def WithAssignment    [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, ASSIGNMENT      )
  def WithPattern       [T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, PATTERN         )
  def WithNiceScriptCall[T](rle: () => Rule1[T]): Rule1[T] = WithScope(rle, NICE_SCRIPT_CALL)

}

trait Settings {
  val settings: Seq[String]
}
