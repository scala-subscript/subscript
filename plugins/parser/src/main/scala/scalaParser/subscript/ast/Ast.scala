package scalaParser.subscript.ast


object Ast extends Ast

trait Ast extends Core
             with Header
             with Operators
             with Terms
             with UtilNodes {
  import Constants._

  case class Literal(content: String) extends LiteralNode

  case class SubScriptCode(modifiers : Seq[String], scriptDefs: Seq[Node]) extends Node {
    def rewrite(implicit context: Context, output: Output) = {
      val dContext = context + (Key.MODIFIERS -> modifiers)
      scriptDefs.map(_.compile(dContext)).mkString("\n")
    }
  }

  case class ScriptDef(header: ScriptHeader, body: Option[Node]) extends Node {
    def rewrite(implicit context: Context, output: Output) = {
      val modifierList = context(Key.MODIFIERS).asInstanceOf[Seq[String]]
      val modifiers = {
        val mods = modifierList.mkString(" ")
        if (!mods.isEmpty) mods + " " else mods
      }

      val bodyStr = body.map {b =>
        val bodyCtx = context ++ Map(
          Key.HEADER_NAME   -> header.name
        , Key.FORMAL_PARAMS -> header.args.flatten.map {
            case FormalConstrainedParameter(_, name, _, _) => Some(s"""$name.~??(Symbol("$name"))""")
            case FormalOutputParameter     (_, name, _, _) => Some(s"""$name.~?(Symbol("$name"))""" )
            case _ => None
          }.filter(_.isDefined).map(_.get)
        )

        s""" = ${b.compile(bodyCtx, output)}"""
      }.getOrElse("")

      s"""${modifiers}def ${header.compile}$bodyStr"""
    }
  }

  case class ScriptBody(expr: Node) extends Node with AspectNode {
    val name = Key.SCRIPT

    def rewriteRaw(implicit context: Context, output: Output) = {
      val headerName   = context(Key.HEADER_NAME  ).asInstanceOf[String]
      val formalParams = context.get(Key.FORMAL_PARAMS).asInstanceOf[Option[Seq[String]]].map {params =>
        if (params.isEmpty) "" else s""", ${params.mkString(", ")}"""
      }.getOrElse("")

      s"""${DSL.Term.SCRIPT}[Any](None, Symbol("$headerName")$formalParams){(${Name.NODE}: ${DSL.Type.SCRIPT}[Any]) =>
         |  implicit val ${Name.SCRIPT} = ${Name.NODE}
         |${expr.compile}}""".stripMargin
    }
  }

}