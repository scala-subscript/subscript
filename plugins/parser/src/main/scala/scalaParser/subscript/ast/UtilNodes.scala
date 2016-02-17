package scalaParser.subscript.ast

import scalaParser.subscript.util._

trait UtilNodes {this: Ast =>
  import Constants.Key._

  trait AspectNode extends Node {
    val name: String

    def rewriteRaw(implicit context: Context, output: Output): String

    def rewrite(implicit context: Context, output: Output): String = {
      val compiled = rewriteRaw

      def aspect(key: String): String = output.last
        .get(key)
        .map {_.asInstanceOf[List[String]]}
        .getOrElse(Nil)
        .mkString("\n")

      val beforeCode = aspect(before(name))
      val afterCode  = aspect(after (name))

      if (!beforeCode.isEmpty || !afterCode.isEmpty)
        s"""{
            |  $beforeCode
            |  $compiled
            |  $afterCode
            |}""".stripMargin
      else compiled
    }    
  }

  trait Communication {this: Node =>

    private var pushedValue: Option[Map[String, Any]] = None

    def compile(implicit t2b: Context = Map(), b2t: Output = new CommunicationStackImpl): String = {
      val initialSize = b2t.stack.size

      val compiledValue = rewrite

      // If something was pushed by this node, push it, otherwise push an empty map
      pushedValue match {
        case Some(v) => b2t push v
        case None    => b2t push Map()
      }
      pushedValue = None  // A single node shouldn't be compiled twice, but just in case...

      // Reduce, so that all the children's and this node's values get combined into one
      b2t.reduce(b2t.stack.size - initialSize)

      compiledValue
    }

    def push(m: Map[String, Any]): Unit = pushedValue = Some(m)
    def push(p: (String, Any)   ): Unit = push(Map(p))

  }

}
