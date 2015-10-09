package scalaParser
import acyclic.file
import org.parboiled2._

import scala.language.implicitConversions


abstract class Core extends Parser
                       with syntax.Basic
                       with syntax.Literals
                       with syntax.Identifiers
                       with RulesOps
                       with Switches
                       with Settings {

  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.
  type R0 = Rule0
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */

  def WSOneR0      : R0 = rule (Basic.WSChar | Literals.CommentR0)

  def WSOneOrMoreR0: R0 = rule (WSOneR0+)
  def WSR0         : R0 = rule (WSOneR0*)
  def WS           : R1 = rule( capture(WSR0) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WLOneR0      : R0 = rule( Basic.WSChar | Literals.CommentR0 | Basic.Newline     )

  def WLOneOrMoreR0: R0 = rule( WLOneR0+      )
  def WLR0         : R0 = rule( WLOneR0*      )
  def WL           : R1 = rule( capture(WLR0) )


  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  private implicit def wspStr(s: String): R1 = rule( WL ~ capture(str(s)) ~> Concat )
  private implicit def wspCh (s: Char  ): R1 = rule( WL ~ capture(ch (s)) ~> Concat )
  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object KeyWordOperators {
    def WR0(s: String): Rule0 = rule( WLR0 ~ Key.W(s) )

    // def W(s: String): R1 = rule( WL ~ capture(Key.W(s)) ~> Concat )
    def W(s: String): R1 = rule( capture(WR0(s)) )
    def O(s: String): R1 = rule( WL ~ capture(Key.O(s)) ~> Concat )
  }
  import KeyWordOperators._
  // R0 versions of certain keywords
  def `withR0` = WR0("with")

  // Keywords that match themselves and nothing else
  def `=>` = rule( O("=>") | O("⇒") )
  def `<-` = rule( O("<-") | O("←") )
  def `:` = O(":")
  def `=` = O("=")
  def `@` = O("@")
  def `_` = W("_")
  def `this` = W("this")
  def `type` = W("type")
  def `val` = W("val")
  def `var` = W("var")
  def `def` = W("def")
  def `with` = W("with")
  def `package` = W("package")
  def `object` = W("object")
  def `class` = W("class")
  def `case` = W("case")
  def `trait` = W("trait")
  def `extends` = W("extends")
  def `implicit` = W("implicit")
  def `try` = W("try")
  def `new` = W("new")
  def `macro` = W("macro")
  def `import` = W("import")
  def `else` = W("else")
  def `super` = W("super")
  def `catch` = W("catch")
  def `finally` = W("finally")
  def `do` = W("do")
  def `yield` = W("yield")
  def `while` = W("while")
  def `<%` = O("<%")
  def `override` = W("override")
  def `#` = O("#")
  def `forSome` = W("forSome")
  def `for` = W("for")
  def `abstract` = W("abstract")
  def `throw` = W("throw")
  def `return` = W("return")
  def `lazy` = W("lazy")
  def `if` = W("if")
  def `match` = W("match")
  def `>:` = O(">:")
  def `<:` = O("<:")
  def `final` =  W("final")
  def `sealed` = W("sealed")
  def `private` = W("private")
  def `protected` = W("protected")

  // SubScript
  def `then` = W("then")

  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `_*`: R1 = rule( `_` ~ "*" ~> Concat )
  def `}` : R1 = rule( Semis.? ~> ExtractOpt ~ '}' ~> Concat )
  def `{` : R1 = rule( '{' ~ (Semis.? ~> ExtractOpt) ~> Concat )
  /**
   * helper printing function
   */
  def pr(s: String) = rule( run(println(s"LOGGING $cursor: $s")) )

  def SemiR0 : R0 = rule( WSR0 ~ Basic.Semi )
  def SemisR0: R0 = rule( SemiR0.+ )

  def SSKeyword: R1

  object IdBank extends ScopeBank[String] {
    import ScopeSwitch._

    def StandardId = rule ( WL ~ capture(Identifiers.Id) ~> Concat )

    def Forbidden(rle: () => Rule1[_]): R1 = rule {!(rle()) ~ StandardId}

    def NormalInScriptTerminator = rule(
      SSKeyword
    | "*}"  
    | "?}"  
    | "!}"  
    | ".}"  
    | "...}"
    | ">>"
    | "==>"
    )

    def LaunchAnchorTerminator = rule (SSKeyword | "**)")
    def LaunchTerminator       = rule (SSKeyword | "*)")

    def ruleMap = Map(
      NORMAL           -> {() => StandardId}
    , NORMAL_IN_SCRIPT -> {() => Forbidden {() => NormalInScriptTerminator}}
    , LAUNCH_ANCHOR    -> {() => Forbidden {() => LaunchAnchorTerminator}}
    , LAUNCH           -> {() => Forbidden {() => LaunchTerminator}}
    , PATTERN          -> NORMAL_IN_SCRIPT
    )
  }
  def Id     : R1 = IdBank().apply()

  def VarId  : R1 = rule( WL ~ capture(Identifiers.VarId) ~> Concat )
  def Literal: R1 = rule( WL ~ Literals.Literal           ~> Concat )
  // def Semi   : R1 = rule( WS ~ capture(Basic.Semi)        ~> Concat )
  // def Semis  : R1 = rule( Semi.+                          ~> ConcatSeqNoDelim )
  def Semi   : R1 = rule( capture(SemiR0 ) )
  def Semis  : R1 = rule( capture(SemisR0) )   
  def Newline: R1 = rule( WL ~ capture(Basic.Newline)     ~> Concat )

  def QualId: R1 = rule( WL ~ OneOrMore(() => Id, () => '.') ~> Concat )
  def Ids   : R1 = rule( OneOrMore(() => Id, () => ',') )

  def NotNewline: R1 = rule( capture(&( WS ~ !Basic.Newline )) )
  def OneNLMax: R1 = {
    def WSChar: R1 = rule( capture(Basic.WSChar.*) )
    def ConsumeComments: R1 = rule( (WSChar ~ Literals.Comment ~ WSChar ~ capture(Basic.Newline) ~> Concat4).* ~> ConcatSeqNoDelim )
    rule( WS ~ capture(Basic.Newline.?) ~ ConsumeComments ~ NotNewline ~> Concat4 )
  }

  def StableId: R1 = {
    def ClassQualifier: R1 = rule( '[' ~ Id ~ ']' ~> Concat3 )
    def ThisSuper     : R1 = rule( `this` | `super` ~ (ClassQualifier.? ~> ExtractOpt) ~> Concat )
    
    object PathDelimiterBank extends ScopeBank[String] {
      import ScopeSwitch._

      override val default = SCRIPT

      def ruleMap = Map(
        NORMAL -> {() => rule           ('.')                   }
      , SCRIPT -> {() => rule(capture(ch('.')) ~ !WLOneOrMoreR0)}

      , NORMAL_IN_SCRIPT -> NORMAL
      , ASSIGNMENT       -> NORMAL
      , PATTERN          -> NORMAL
      )
    }
    def PathDelimiter   = PathDelimiterBank().apply()

    def IdsTail         = rule { (PathDelimiter ~ Id ~> Concat).* ~> ConcatSeqNoDelim }
    def ThisSuperTailed = rule { ThisSuper ~ IdsTail    ~> Concat           }

    rule(
        (Id ~ PathDelimiter ~> Concat).* ~> ConcatSeqNoDelim ~ ThisSuperTailed ~> Concat
      | Id ~ IdsTail ~> Concat
    )
  }
}