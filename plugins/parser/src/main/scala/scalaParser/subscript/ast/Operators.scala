package scalaParser.subscript.ast


trait Operators {this: Ast =>
  import Constants.DSL.Op._
  import Constants._

  trait ScriptOperator extends Node {
    val method: String
  }

  trait ScriptOperatorNary extends ScriptOperator {
    val operands: Seq[Node]

    override def rewrite(implicit context: Context, output: Output): String =
      if (operands.size > 1) {
        def operandsStr = operands.map(_.compile).mkString(", ")
        s"$method($operandsStr)"
      }
      // If there's just one operand, compile it without wrapping in a DSL method
      // If there are no operands, return an empty string
      else operands.headOption.map(_.compile).getOrElse("")
  }

  case class IdentityOp(node: Node) extends Expr7 with IdentityNode {
    val operands = Nil
    val method   = ""
  }


  trait ExprSeq extends ScriptOperatorNary {val method = SEQUENCE}

  trait Expr9 extends Node
  case class Expr9Seq     (operands: Seq[Node]) extends Expr9 with ExprSeq
  case class Expr9Identity(node    :     Node ) extends Expr9 with IdentityNode

  case class Expr8(operands: Seq[Node]) extends ExprSeq

  trait Expr7 extends ScriptOperator

  trait IfExprBase extends Expr7 {
    val nIf: Node

    def condition: String = 
      s"""$method (${Name.NODE} => {
         |  implicit val ${Name.HERE} = ${Name.NODE}
         |${nIf.compile}
         |})""".stripMargin
  }

  case class IfExpr(nIf: Node, nThen: Node) extends IfExprBase {
    val method = IF_EXPR

    def rewrite(implicit context: Context, output: Output): String =
      s"$condition(${nThen.compile})"
  }

  case class IfElseExpr(nIf: Node, nThen: Node, nElse: Node) extends IfExprBase {
    val method = IF_ELSE_EXPR

    def rewrite(implicit context: Context, output: Output): String =
      s"$condition(${nThen.compile}, ${nElse.compile})"
  }

  case class DoThen    (operands: Seq[Node]) extends Expr7 with ScriptOperatorNary {val method = DO_THEN     }
  case class DoThenElse(operands: Seq[Node]) extends Expr7 with ScriptOperatorNary {val method = DO_THEN_ELSE}
  case class DoElse    (operands: Seq[Node]) extends Expr7 with ScriptOperatorNary {val method = DO_ELSE     }

  trait Expr6 extends ScriptOperatorNary
  case class OrPar1(operands: Seq[Node]) extends Expr6 {val method = OR_PAR_1}
  case class OrPar2(operands: Seq[Node]) extends Expr6 {val method = OR_PAR_2}

  trait Expr5 extends ScriptOperatorNary
  case class AndPar1(operands: Seq[Node]) extends Expr5 {val method = AND_PAR_1}
  case class AndPar2(operands: Seq[Node]) extends Expr5 {val method = AND_PAR_2}

  trait Expr4 extends ScriptOperatorNary
  case class Equality(operands: Seq[Node]) extends Expr4 {val method = EQUALITY}

  trait Expr3 extends ScriptOperatorNary
  case class Alternative(operands: Seq[Node]) extends Expr3 {val method = ALTERNATIVE}

  trait Expr2 extends ScriptOperatorNary
  case class Disrupt         (operands: Seq[Node]) extends Expr2 {val method = DISRUPT            }
  case class Shuffle         (operands: Seq[Node]) extends Expr2 {val method = SHUFFLE            }
  case class Shuffle1OrMore  (operands: Seq[Node]) extends Expr2 {val method = SHUFFLE_1_OR_MORE  }
  case class Interrupt       (operands: Seq[Node]) extends Expr2 {val method = INTERRUPT          }
  case class Interrupt0OrMore(operands: Seq[Node]) extends Expr2 {val method = INTERRUPT_0_OR_MORE}

  case class Expr1(operands: Seq[Node]) extends ExprSeq


  trait DataflowTerm extends ScriptOperator {
    def matcher(id: String, tpe: String, rhs: String): String =
      s"(_$id: Any) => _$id match {case $id: $tpe => $rhs}"    
  }

  case class DataflowThenElse(lhs: Node, id: String, tpe: String, rhs: Node, eId: String, eTpe: String, eRhs: Node) extends DataflowTerm {
    val method = DATAFLOW_THEN_ELSE

    def rewrite(implicit context: Context, output: Output): String = {
      val lhsStr  = nodeToScript("~~>"      , lhs )
      val rhsStr  = nodeToScript(Name.LAMBDA, rhs )
      val eRhsStr = nodeToScript(Name.LAMBDA, eRhs)

      s"""$method(
         |  $lhsStr
         |, ${matcher(id , tpe , rhsStr )}
         |, ${matcher(eId, eTpe, eRhsStr)}
         |)""".stripMargin
    }
  }

  case class DataflowThen(lhs: Node, id: String, tpe: String, rhs: Node) extends DataflowTerm {
    val method = DATAFLOW_THEN

    def rewrite(implicit context: Context, output: Output): String = {
      val lhsStr  = nodeToScript("~~>"      , lhs )
      val rhsStr  = nodeToScript(Name.LAMBDA, rhs )

      s"""$method(
         |  $lhsStr
         |, ${matcher(id , tpe , rhsStr )}
         |)""".stripMargin
    }
  }

  case class DataflowEmpty(node: Node) extends DataflowTerm with IdentityNode {val method = null}

  // === New dataflow ===
  case class Dataflow(nDo: Node, nThen: Seq[DataflowClause], nElse: Seq[DataflowClause]) extends ScriptOperator {
    val method = DATAFLOW

    def rewrite(implicit context: Context, output: Output): String = {
      val nDoStr       = nodeToScript("~~>", nDo)
      val nThenClauses = nThen.map(_.compile).mkString("\n")
      val nElseClauses = nElse.map(_.compile).mkString("\n")

      val defaultMatcher = """case _ => throw new RuntimeException("No suitable matcher found")"""
      def block(content: String) =
        s"""{
           |$content
           |${if (content.isEmpty) defaultMatcher else ""}
           |}""".stripMargin

      s"""$method(
         |  $nDoStr
         |, ${block(nThenClauses)}
         |, ${block(nElseClauses)}
         |)""".stripMargin
    } 
  }

  case class DataflowClause(pattern: String, expr: Node) extends Node {
    def rewrite(implicit context: Context, output: Output): String = {
      val exprStr = nodeToScript(Name.LAMBDA, expr)
      s"case $pattern => $exprStr"
    }
  }

  trait Term extends Node
  case class Parenthesised(node: Node) extends Term with IdentityNode
  case class Launch       (node: Node) extends Term with  WrappedNode {val method = LAUNCH       }
  case class LaunchAnchor (node: Node) extends Term with  WrappedNode {val method = LAUNCH_ANCHOR}

}