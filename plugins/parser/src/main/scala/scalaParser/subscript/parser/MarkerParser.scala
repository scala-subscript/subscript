package scalaParser.subscript.parser

import language.implicitConversions

import org.parboiled2._

import scalaParser._
import scalaParser.syntax._

import scalaParser.subscript.ast.Constants


/**
 * Determines whether a given file is subscript-containing or not.
 */
class MarkerParser (input: ParserInput) extends Scala(input) {

  type ImportModel = String

  def ExtractImports: Rule1[Seq[ImportModel]] = {
    def Trans1: (String, Option[Seq[ImportModel]]) => Seq[ImportModel] = (_, x) => Trans2(x)
    def Trans2:          Option[Seq[ImportModel]]  => Seq[ImportModel] = _.getOrElse(Seq())

    def Imports:       Rule1[Seq[ImportModel]] = rule { Import + SemisR0 }
    def TopPackageSeq: Rule1[String          ] = OneOrMore(() => rule (`package` ~ QualId ~ !(WS ~ "{") ~> Concat), () => Semis)
    def Body         : Rule1[Seq[ImportModel]] = rule( TopPackageSeq ~ (SemisR0 ~ Imports).? ~> Trans1 | Imports.? ~> Trans2 )
    
    rule( SemisR0.? ~ Body )
  }

  def Identify: Rule1[Boolean] = {

    def Trans1: Seq[ImportModel] => Boolean = imports => imports.exists(_ endsWith Constants.Name.FILE)

    rule( ExtractImports ~> Trans1 )

  }
}