package scalaParser.subscript.ast

import scalaParser.subscript.util._

trait Core {this: Ast =>

  type Context = Map[String, Any]
  type Output  = CommunicationStack

  trait Node extends Communication {
    def rewrite(implicit context: Context, output: Output): String
  }

  trait LiteralNode extends Node {
    val content: String
    def rewrite(implicit context: Context, output: Output) = content
  }

  trait IdentityNode extends Node {
    val node: Node
    override def rewrite(implicit context: Context, output: Output): String = node.compile
  }

  trait WrappedNode extends Node {
    val node  : Node
    val method: String

    override def rewrite(implicit context: Context, output: Output): String = s"$method(${node.compile})"
  }

}