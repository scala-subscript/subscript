package scalaParser.subscript.ast


trait Header {this: Ast =>
  import Constants.DSL.Type._

  case class ScriptHeader(name: String, args: Seq[Seq[Parameter]], implicitArgs: Option[Seq[Parameter]], tpe: Option[String]) extends Node {
    def rewrite(implicit context: Context, output: Output): String = {
      def compileArgs(args: Seq[Parameter]): String = args.map(_.compile).mkString(", ")

      val explicitArgsStr: String = args
        .map(compileArgs)
        .map {args => s"($args)"}
        .mkString

      val implicitArgsStr: String =
        implicitArgs.map {args => s"(implicit ${compileArgs(args)})"}.getOrElse("")

      val argsStr: String = explicitArgsStr + implicitArgsStr

      val tpeStr: String = tpe.map {t => s": $SCRIPT_NODE[$t]"}.getOrElse("")

      s"$name$argsStr$tpeStr"
    }
  }

  trait Parameter extends Node {
    val name      : String
    val tpe       : String
    val annots    : String
    val defaultVal: Option[String]


    def tpeStr(transform: String => String): String = s": ${transform(tpe)}"

    def wrappedTpeStr(enclosingType: String) = tpeStr {t => s"$enclosingType[$t]"}
    def pureTpeStr = tpeStr(x => x)

    def annotsStr: String = if (!annots.isEmpty) annots + " " else ""
    def defaultValStr: String = defaultVal.map {v => s" = $v"}.getOrElse("")
  }

  case class NormalParameter(annots: String, name: String, tpe: String, defaultVal: Option[String]) extends Parameter {
    def rewrite(implicit context: Context, output: Output): String = s"$annotsStr$name$pureTpeStr$defaultValStr"
  }

  case class FormalOutputParameter(annots: String, name: String, tpe: String, defaultVal: Option[String]) extends Parameter {
    def rewrite(implicit context: Context, output: Output): String = s"$annotsStr$name${wrappedTpeStr(OUTPUT_PARAMETER)}$defaultValStr"
  }

  case class FormalConstrainedParameter(annots: String, name: String, tpe: String, defaultVal: Option[String]) extends Parameter {
    def rewrite(implicit context: Context, output: Output): String = s"$annotsStr$name${wrappedTpeStr(FORMAL_CONSTRAINED_PARAMETER)}$defaultValStr"
  }

  case class ConstrainableOutputParameter(annots: String, name: String, tpe: String, defaultVal: Option[String]) extends Parameter {
    def rewrite(implicit context: Context, output: Output): String = s"$annotsStr$name${wrappedTpeStr(CONSTRAINABLE_OUTPUT_PARAMETER)}$defaultValStr"
  }

}
