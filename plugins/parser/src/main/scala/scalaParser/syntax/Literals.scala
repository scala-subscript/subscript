package scalaParser
package syntax
import acyclic.file
import org.parboiled2._

trait Literals { self: Parser with Basic with Identifiers with RulesOps =>
  type R1 = Rule1[String]

  def Block: R1
  def WL: R1
  object Literals{
    import Basic._
    def Float: R1 = {
      def Thing = rule( Digit.+ ~ Exp.? ~ FloatType.? )
      def Thing2 = rule( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      rule( capture("." ~ Thing | Digit.+ ~ Thing2) )
    } 

    def Int: R1 = rule( capture((HexNum | DecNum) ~ anyOf("Ll").?) )

    def Bool: R1 = rule( capture(Key.W("true") | Key.W("false"))  )

    def MultilineCommentR0: Rule0 = rule( "/*" ~ (MultilineCommentR0 | !"*/" ~ ANY).* ~ "*/" )
    // def MultilineComment  : R1 = rule( capture("/*") ~ ((MultilineComment | capture(!"*/" ~ ANY)).* ~> ConcatSeqNoDelim) ~ capture("*/") ~> Concat3 )
    def MultilineComment: R1 = rule ( capture(MultilineCommentR0) )

    def CommentR0: Rule0 = rule(
      MultilineCommentR0 | "//" ~ (!Basic.Newline ~ ANY).* ~ &(Basic.Newline | EOI)
    ) 
    // def Comment: R1 = rule(
    //   MultilineComment | capture("//" ~ (!Basic.Newline ~ ANY).* ~ &(Basic.Newline | EOI))
    // )
    def Comment: R1 = rule ( capture(CommentR0) )


    def Null: R1 = rule( capture(Key.W("null")) )
    def Literal: R1 = rule( (capture("-".?) ~ (Float | Int) ~> Concat) | Bool | Char | String | Symbol | Null )

    def EscapedChars: R1 = rule( capture('\\' ~ anyOf("btnfr'\\\"")) )

    // Note that symbols can take on the same values as keywords!
    def Symbol: R1 = rule( capture(''' ~ (Identifiers.PlainId | Identifiers.Keywords)) )

    def Char: R1 = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPredicate.from(isPrintableChar)

      rule {
        capture("'") ~ (capture(UnicodeEscape) | EscapedChars | capture(!'\\' ~ PrintableChar)) ~ capture("'") ~> Concat3
      }
    }


    def pr(s: String): Rule0 = rule( run(println(s"LOGGING $cursor: $s")) )
    def Interp: R1 = rule{
      capture("$" ~ Identifiers.PlainIdNoDollar) | capture("${") ~ Block ~ WL ~ capture("}") ~> Concat4 | capture("$$")
    }
    def String: R1 = {
      import Identifiers.Id
      def InterpIf(b: Boolean): R1 = if(b) rule(Interp) else rule(capture(MISMATCH0))
      def TQ: Rule0 = rule( "\"\"\"" )
      def TripleChars(b: Boolean) = rule( (InterpIf(b) | capture('"'.? ~ '"'.? ~ noneOf("\"")) ).* ~> ConcatSeqNoDelim )
      def TripleTail: R1 = rule( capture(TQ ~ (zeroOrMore('"'))) )
      def SingleChars(b: Boolean): R1 = rule( ( InterpIf(b) | capture("\\\"") | capture("\\\\") | capture(noneOf("\n\"")) ).* ~> ConcatSeqNoDelim )
      rule {
        (capture(Id ~ TQ ) ~ TripleChars(b = true ) ~ TripleTail   ~> Concat3) |
        (capture(Id ~ '"') ~ SingleChars(b = true ) ~ capture('"') ~> Concat3) |
        (capture(TQ )      ~ TripleChars(b = false) ~ TripleTail   ~> Concat3) |
        (capture('"')      ~ SingleChars(b = false) ~ capture('"') ~> Concat3)
      }
    }

    def isPrintableChar(c: Char): Boolean = {
      val block = Character.UnicodeBlock.of(c)
      !Character.isISOControl(c) && !Character.isSurrogate(c) && block != null && block != Character.UnicodeBlock.SPECIALS
    }
  }
}