package scalaParser.subscript.ast

trait CodePrimitives {this: Ast =>
  def nodeToScript(name: String, node: Node): String = ScriptBody(node).compile(
    t2b = Map(Constants.Key.HEADER_NAME -> name)
  )

  def partialFunction(content: String) = {
    val defaultMatcher = """case _ => throw new RuntimeException("No suitable matcher found")"""
    s"""{
       |$content
       |${if (content.isEmpty) defaultMatcher else ""}
       |}""".stripMargin
   }
}