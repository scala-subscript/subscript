package scalaParser.subscript.ast

import scalaParser.subscript.util.CommunicationStackImpl


trait Terms {this: Ast =>
  import Constants._
  import Constants.DSL._

  // Script calls
  case class ScriptCall(content: Node) extends Term {
    val method = "subscript.DSL._maybeCall"

    def rewrite(implicit context: Context, output: Output): String = {
      val str = s"${Term.VAR_CALL}(${Ast.metaString(content.compile)})"
      s"""$method("", (${Name.HERE}: ${Type.CALL_GRAPH_TREE_NODE}) => $str)"""  
    }
  }


  // Code fragments
  trait CodeFragment extends Term {
    val code  : String
    val method: String

    def rewrite(implicit context: Context, output: Output): String =
      s"""$method[Any] (${Name.NODE} => {
         |  implicit val ${Name.HERE} = ${Name.NODE}
         |$code
         |}, true)""".stripMargin
  }

  case class Normal           (code: String) extends CodeFragment {val method = Term.NORMAL            }
  case class Threaded         (code: String) extends CodeFragment {val method = Term.THREADED          }
  case class Unsure           (code: String) extends CodeFragment {val method = Term.USURE             }
  case class Tiny             (code: String) extends CodeFragment {val method = Term.TINY              }
  case class Eventhandling    (code: String) extends CodeFragment {val method = Term.EVENTHANDLING     }
  case class EventhandlingLoop(code: String) extends CodeFragment {val method = Term.EVENTHANDLING_LOOP}

  case class Annotation(code: Node, annotee: Node) extends Term {
    val method = Term.ANNOTATION

    override def rewrite(implicit context: Context, output: Output): String = {
      val (nType, tType) = annotee match {
        case Normal           (_) => (s"${Type.N_NORMAL}[Any]"            , s"${Type.T_NORMAL}[Any]"            )
        case Threaded         (_) => (s"${Type.N_THREADED}[Any]"          , s"${Type.T_THREADED}[Any]"          )
        case Unsure           (_) => (s"${Type.N_UNSURE}[Any]"            , s"${Type.T_UNSURE}[Any]"            )
        case Tiny             (_) => (s"${Type.N_TINY}[Any]"              , s"${Type.T_TINY}[Any]"              )
        case Eventhandling    (_) => (s"${Type.N_EVENTHANDLING}[Any]"     , s"${Type.T_EVENTHANDLING}[Any]"     )
        case EventhandlingLoop(_) => (s"${Type.N_EVENTHANDLING_LOOP}[Any]", s"${Type.T_EVENTHANDLING_LOOP}[Any]")

        case _                    => (Type.CALL_GRAPH_NODE, Type.TEMPLATE_CHILD)
      }
      s"""$method[$nType, $tType](${Name.HERE} => {
         |  implicit val ${Name.THERE}: $nType = here.there;
         |${code.compile}
         |}).apply(${annotee.compile})""".stripMargin
    }
  }

  // Declarations
  abstract class Declaration(method: String, nodeTpe: String) extends Term {
    val id        : String
    val tpe       : Option[String]
    val expression: Node

    def rewrite(implicit context: Context, output: Output): String = {
      // Type of the variable
      val tpeStr     = tpe

      val exprString  = expression.compile
      val metaExprStr = metaString(exprString)

      val result = {
        val valueCode = tpe match {
          case Some(tpeStr) => s"(${Name.NODE}: $nodeTpe[$tpeStr]) => {implicit val ${Name.HERE} = ${Name.NODE}; val tr: $tpeStr = $exprString; tr}"   // tr = typedReturn
          case None         => s"${Term.UNTYPED_VALUE_CODE}($metaExprStr)"
        }
        s"$method($id, $valueCode)"
      }

      val decl = {
        val header = s"val $id = "
        val body   = tpe match {
          case Some(tpeStr) => s"""${Term.DECLARE}[$tpeStr](scala.Symbol("$id"))"""
          case None         => s"""${Term.UNTYPED_DECLARE}($metaExprStr, scala.Symbol("$id"))"""
        }
        header + body
      }

      push(Key.before(Key.SCRIPT) -> decl)
      result
    }
  }

  case class VarDecl(id: String, tpe: Option[String], expression: Node) extends Declaration(Term.VAR, Type.LOCAL_VAR)
  case class ValDecl(id: String, tpe: Option[String], expression: Node) extends Declaration(Term.VAL, Type.LOCAL_VAR)


  // Special leafs
  trait Special extends Term

  class SpecialConstant(method: String) extends Special {
    def rewrite(implicit context: Context, output: Output): String = method
  }

  case object Delta             extends SpecialConstant(Term.DELTA              )
  case object Epsilon           extends SpecialConstant(Term.EPSILON            )
  case object Neutral           extends SpecialConstant(Term.NEUTRAL            )
  case object Loop              extends SpecialConstant(Term.LOOP               )
  case object OptionalBreakLoop extends SpecialConstant(Term.OPTIONAL_BREAK_LOOP)
  case object OptionalBreak     extends SpecialConstant(Term.OPTIONAL_BREAK     )
  case object Break             extends SpecialConstant(Term.BREAK              )

  case class While(condition: String) extends Special {
    def rewrite(implicit context: Context, output: Output): String =
      s"""${Term.WHILE} (${Name.NODE} => {
         |  implicit val ${Name.HERE} = ${Name.NODE}
         |$condition
         |})""".stripMargin

  }


  // Actors
  trait ActorClause extends Node {
    val condition: String
    val code     : Option[String]
    val script   : Option[Node]

    def rewrite(implicit context: Context, output: Output): String = {
      val codeStr  : String = code  .getOrElse(" =>")
      val scriptStr: String = script.map(_.compile(Map(Key.HEADER_NAME -> Name.LAMBDA), new CommunicationStackImpl )).getOrElse("null")
      
      s"""$condition$codeStr
         |$scriptStr""".stripMargin
    }
  }
  case class ActorCaseClause (condition: String, code: Option[String], script: Option[Node]) extends ActorClause
  case class ActorShortClause(condition: String, code: Option[String], script: Option[Node]) extends ActorClause {
    override def rewrite(implicit context: Context, output: Output): String =
      s"case ${super.rewrite}"
  }

  case class ActorCall(clauses: Seq[ActorClause]) extends Term {
    def rewrite(implicit context: Context, output: Output): String = {
      val body: String = clauses.map(_.compile).mkString("\n")
      s"""${Name.ACTOR_CALL}({
         |$body
         |})""".stripMargin
    }
  }
}
