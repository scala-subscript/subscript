package scalaParser.subscript.parser

import language.implicitConversions
import org.parboiled2._
import scalaParser._
import scalaParser.syntax._

import scalaParser.subscript.ast.Ast


/**
 * Parser for Scala syntax.
 */
trait SubScript extends Core with HighPriorityRulesConversions
                   with Header
                   with Operators {this: Exprs =>

  def SubScriptCode: R1 = rule {SubScriptCodeAst ~> {n: Ast.SubScriptCode => n.compile()}}

  def SubScriptCodeAst: R[Ast.SubScriptCode] = {
    
    def Trans1: (Seq[String], String, Seq[Ast.ScriptDef]) => Ast.SubScriptCode =
      {(mod, _, defs) => Ast.SubScriptCode(mod, defs)}
    
    def Trans2:              Ast.ScriptDef   => Seq[Ast.ScriptDef] =        x => Seq(x)
    def Trans3: (String, Seq[Ast.ScriptDef]) => Seq[Ast.ScriptDef] = (_, seq) => seq

    def Trans4: (String, Seq[String]) => Seq[String] = (head, tail) => head :: tail.toList
    def Trans5: Option[Seq[String]] => Seq[String] = _.getOrElse(Nil)

    lazy val minIndent = col + 2
    rule {
      WLR0 ~ Code {minIndent} ~ (Mod ~ (WLR0 ~ Mod).* ~> Trans4).? ~> Trans5 ~ `script` ~ (ScriptDef ~> Trans2 | `..` ~ IndentedNLSequence (() => ScriptDef, minIndent) ~> Trans3) ~> Trans1
    }
  }

  def ScriptDef: R[Ast.ScriptDef] = {

    def Trans1: (Ast.ScriptHeader, Option[Ast.Node]) => Ast.ScriptDef =
      (header, maybeExpr) => Ast.ScriptDef(header, maybeExpr)

    def Trans2: (String, Ast.Node) => Ast.Node =
      (_, expr) => expr

    rule {
      WLR0 ~ ScriptHeader ~ (`=` ~ ScriptBody ~> Trans2).? ~> Trans1
    }
  }

  def ScriptBody: R[Ast.ScriptBody] = rule { WithScript {() => Expr9} ~> Ast.ScriptBody }
}