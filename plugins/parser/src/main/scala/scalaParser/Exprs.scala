package scalaParser
// import acyclic.file

import scalaParser.subscript.parser.SubScript
import scalaParser.subscript.ast.Ast
import scalaParser.subscript.ast.Constants


/**
 * Created by haoyi on 11/30/14.
 */
trait Exprs extends Core with Types with Xml with SubScript {

  private implicit def wspStr(s: String): R1 = rule( WL ~ capture(str(s)) ~> Concat )
  private implicit def wspCh (s: Char  ): R1 = rule( WL ~ capture(ch (s)) ~> Concat )

  def NewBody : R1
  def BlockDef: R1

  def Import: R1 = {
    def ImportExpr: R1 = rule( StableId ~ (('.' ~ (`_` | Selectors) ~> Concat).? ~> ExtractOpt) ~> Concat )
    def Selectors: R1 = rule( '{' ~ ((Selector ~ ',' ~> Concat).* ~> ConcatSeqNoDelim) ~ (Selector | `_`) ~ "}" ~> Concat4 )
    def Selector: R1 = rule( Id ~ ((`=>` ~ (Id | `_`) ~> Concat).? ~> ExtractOpt) ~> Concat )
    // !!! ',' can't be implicitly converted to have WL behind it, since it is R1.
    rule( `import` ~ OneOrMore(() => ImportExpr, () => ',') ~> Concat )
  }

  def Ascription: R1 = rule( `:` ~ (`_*` |  Type | Annot.+ ~> ConcatSeqNoDelim) ~> Concat )

  def LambdaHead: R1 = {
    def Binding: R1 = rule( (Id | `_`) ~ ((`:` ~ Type ~> Concat).? ~> ExtractOpt) ~> Concat )
    def Bindings: R1 = rule( '(' ~ ZeroOrMore(() => Binding, () => ',') ~ ')' ~> Concat3 )
    def Implicit: R1 = rule( `implicit`.? ~> ExtractOpt ~ Id ~ ((`:` ~ InfixType ~> Concat).? ~> ExtractOpt) ~> Concat3 )
    rule( (Bindings | Implicit | `_` ~ (Ascription.? ~> ExtractOpt) ~> Concat) ~ `=>` ~> Concat )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  def TypeExpr: R1 = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    def OneSemiMax: R1 = if (injectSemicolons) OneNLMax   else rule {capture(MATCH)}
    def NoSemis   : R1 = if (injectSemicolons) NotNewline else rule {capture(MATCH)}

    def Enumerators: R1 = {
      def Generator: R1 = rule( Pat1 ~ `<-` ~ Expr ~ (Guard.? ~> ExtractOpt) ~> Concat4 )
      def Assign: R1 = rule( Pat1 ~ `=` ~ Expr ~> Concat3 )
      def Enumerator: R1 = rule( Semis ~ Generator ~> Concat | optional(Semis) ~> ExtractOpt ~ Guard ~> Concat | Semis ~ Assign ~> Concat )
      rule( Generator ~ (Enumerator.* ~> ConcatSeqNoDelim) ~ WL ~> Concat3 )
    }
    def Expr: R1 = {
      def If: R1 = {
        def Else: R1 = rule( Semi.? ~> ExtractOpt ~ `else` ~ Expr ~> Concat3 )
        rule( `if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ (Else.? ~> ExtractOpt) ~> Concat6 )
      }
      def While: R1 = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr ~> Concat5 )
      def Try: R1 = {
        def Catch: R1 = rule( `catch` ~ Expr ~> Concat )
        def Finally: R1 = rule( `finally` ~ Expr ~> Concat )
        rule( `try` ~ Expr ~ (Catch.? ~> ExtractOpt) ~ (Finally.? ~> ExtractOpt) ~> Concat4 )
      }
      def DoWhile: R1 = rule( `do` ~ Expr ~ (Semi.? ~> ExtractOpt) ~ `while` ~ '(' ~ Expr ~ ")" ~> Concat7 )

      def For: R1 = {
        def Body: R1 = rule( '(' ~ ExprCtx.Enumerators ~ ')' ~> Concat3 | '{' ~ StatCtx.Enumerators ~ '}' ~> Concat3 )
        rule( `for` ~ Body ~ (`yield`.? ~> ExtractOpt) ~ Expr ~> Concat4 )
      }
      def Throw: R1 = rule( `throw` ~ Expr ~> Concat )
      def Return: R1 = rule( `return` ~ (Expr.? ~> ExtractOpt) ~> Concat )

      def SmallerExpr: R1 = rule( PostfixExpr ~ ((`match` ~ '{' ~ CaseClauses ~ "}" ~> Concat4 | Ascription).? ~> ExtractOpt) ~> Concat )
      def LambdaBody: R1 = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      rule( LambdaHead.* ~> ConcatSeqNoDelim ~ LambdaBody ~> Concat )
    }

    def PostfixExpr: R1 = {
      // ! negation !!!
      def Prefixed: R1 = rule( (WL ~ capture(anyOf("-+~!")) ~ WS ~ !Basic.OpChar ~> Concat3) ~  SimpleExpr ~> Concat )
      
      object AssignBank extends ScopeBank[String] with CommonHelpers {
        import ScopeSwitch._

        def StandardWithTransform(transform: (String, String, String) => String): () => R1 =
          {() => rule ( WithNormal {() => SimpleExpr} ~ `=` ~ Expr ~> transform | SimpleExpr)}

        def AssignMethod: (String, String, String) => String = (lhs, _, expr) =>
          s"${scalaParser.subscript.ast.Constants.DSL.Term.VAR_ASSIGNMENT}(${metaString(lhs)},$expr)"

        override val default = NORMAL_IN_SCRIPT
        def ruleMap = Map(
          NORMAL           -> {() => rule (                       SimpleExpr  ~ `=` ~ Expr ~> Concat3      | SimpleExpr)}
        , NORMAL_IN_SCRIPT -> {() => rule ( WithAssignment {() => SimpleExpr} ~ `=` ~ Expr ~> AssignMethod | SimpleExpr)}
        )
      }
      def Assign: R1 = AssignBank().apply()

      def PrefixExpr: R1 = rule( Prefixed | Assign )

      def InfixExpr: R1 = rule( PrefixExpr ~ ((NoSemis ~ Id ~ (TypeArgs.? ~> ExtractOpt) ~ OneSemiMax ~ PrefixExpr ~> Concat5).* ~> ConcatSeqNoDelim) ~> Concat )

      
      object PostfixExprBank extends ScopeBank[String] with CommonHelpers {
        import ScopeSwitch._

        def Normal: R1 = rule( InfixExpr ~ ((NotNewline ~ Id ~ (Newline.? ~> ExtractOpt) ~> Concat3).? ~> ExtractOpt) ~> Concat )

        def Wrap: (String, String) => String = (ws, str) =>
          s"${ws}subscript.DSL._maybeVarCall(${metaString(str)})"
        
        override def default = NORMAL_IN_SCRIPT
        def ruleMap = Map(
          NORMAL           -> {() => Normal}
        , NORMAL_IN_SCRIPT -> {() => rule(WL ~ Normal ~> Wrap)}
        )
      }

      PostfixExprBank().apply()
    }

    def SimpleExpr: R1 = {
      def Path: R1 = rule( (Id ~ '.' ~> Concat).* ~> ConcatSeqNoDelim ~ `this` ~ (('.' ~ Id ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat3 | StableId )
      def New: R1 = rule( `new` ~ NewBody ~> Concat )
      def Parened: R1 = rule ( '(' ~ (Exprs.? ~> ExtractOpt) ~ ")" ~> Concat3  )

      def Trans1: Ast.ScriptBody => String           = _.compile(t2b = Map(Constants.Key.HEADER_NAME -> "lambda"))
      def Trans2: (String, String, String) => String = (_, body, _) => body

      def ScriptLambdaBody: R1 = rule( '[' ~ (ScriptBody ~> Trans1) ~ ']' ~> Trans2 )

      def SimpleExpr1: R1 = rule( XmlExpr | ScriptLambdaBody | New | BlockExpr | Literal | Path | `_` | Parened)
      
      def NormalSimpleExpr    : R1 = rule( SimpleExpr1 ~ NormalSimpleExprTail ~> Concat )
      def NormalSimpleExprTail: R1 = rule { (('.' ~ Id ~> Concat | TypeArgs | NoSemis ~ ArgList ~> Concat).* ~> ConcatSeqNoDelim) ~ ((NoSemis  ~ `_` ~> Concat).? ~> ExtractOpt) ~> Concat }

      object SimpleExprBank extends ScopeBank[String] with CommonHelpers {
        import ScopeSwitch._
        import scalaParser.subscript.ast.Constants.DSL.Term.{VAR_CALL, VAR_ASSIGNMENT}
        import scalaParser.subscript.ast.Constants.DSL.Type.{ACTUAL_OUTPUT_PARAMETER, ACTUAL_ADAPTING_PARAMETER}
        
        def isAlpha(c: Char) =
          c >= 'a' && c <= 'z' ||
          c >= 'A' && c <= 'Z'

        // Can be a local lambda call
        def LocalVar: R1 = rule {Id ~ !(TypeArgs | (NoSemis ~ ArgList ~> Concat) | ch('"'))}
        // def LocalVarTrans: (String, String, String) => String = (spaces, x, tail) => if (isAlpha(x.head)) s"$spaces${VAR_CALL}($x)$tail" else x + tail

        def LocalVarTail  : R1 = rule {!(TypeArgs | NoSemis ~ ArgList ~> Concat) ~ NormalSimpleExprTail}

        def ParticleStartSymbols: R1 = rule {'.' | TypeArgs | NoSemis ~ ArgList ~> Concat}
        def Particle            : R1 = rule {'.' ~ Id ~ &(ParticleStartSymbols) ~> Concat | TypeArgs | NoSemis ~ ArgList ~> Concat}
        def LocalVarTailId      : R1 = rule {!(TypeArgs | NoSemis ~ ArgList ~> Concat) ~ Particle.* ~> ConcatSeqNoDelim ~ '.' ~ Id ~> Concat3}

        // def AssignSimpleExpr = rule(WL ~ LocalVar ~ LocalVarTailId ~> LocalVarTrans)

        def SSId: R1 = rule {!SSKeyword ~ capture(Identifiers.Id)}

        def ActualOutputParameterTrans: (String, String, String) => String = (wl, _, x) =>
          if (isAlpha(x.head)) s"${wl}subscript.DSL._actualOutputParameter(${metaString(x)})" //s"$wl$ACTUAL_OUTPUT_PARAMETER($VAR_CALL(${metaString(x)}), x1 => $VAR_ASSIGNMENT(${metaString(x)}, x1))"
          else x

        def ActualAdaptingParameterTrans: (String, String, String) => String = (wl, _, x) =>
          if (isAlpha(x.head)) s"$wl$ACTUAL_ADAPTING_PARAMETER($x)"
          else x

        override val default = NORMAL_IN_SCRIPT
        def ruleMap = Map(
          NORMAL           -> {() => NormalSimpleExpr}
        , NORMAL_IN_SCRIPT -> {() => rule (
              WL ~ "??"     ~ SSId          ~> ActualAdaptingParameterTrans
            | WL ~ '?'      ~ SSId          ~> ActualOutputParameterTrans
            | NormalSimpleExpr
            )
          }
        
        // , ASSIGNMENT       -> {() => rule (WithNormalInScript {() => AssignSimpleExpr} | WithNormalInScript {() => NormalSimpleExpr})}
        , ASSIGNMENT -> {() => rule (WithNormalInScript {() => NormalSimpleExpr})}

        , NICE_SCRIPT_CALL -> {() => rule(WL ~ WithNormalInScript {() => NORMAL_IN_SCRIPT()} ~> {(wl: String, s: String) => s"${wl}subscript.DSL._maybeVarCall(${metaString(s)})"})}
        )
      }
      SimpleExprBank().apply()
    }
    def Guard : R1 = rule( `if` ~ PostfixExpr ~> Concat )
  }
  def SimplePat: R1 = {
    def ExtractorArgs: R1 = rule( ZeroOrMore(() => Pat, () => ',') )
    def Extractor: R1 = rule( StableId ~ (('(' ~ ExtractorArgs ~ ')' ~> Concat3).? ~> ExtractOpt) ~> Concat )
    def TupleEx: R1 = rule( '(' ~ (ExtractorArgs.? ~> ExtractOpt) ~ ')' ~> Concat3 )
    def Thingy: R1 = rule( `_` ~ ((`:` ~ TypePat ~> Concat).? ~> ExtractOpt) ~ !"*" ~> Concat )  // !"*" doesn't have an effect anyway, hence Concat and not Concat3
    rule( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  def BlockExpr: R1 = rule( '{' ~ (CaseClauses | Block) ~ `}` ~> Concat3 )

  def BlockStats: R1 = {
    def Prelude: R1 = rule( Annot.* ~> ConcatSeqNoDelim ~ (`implicit`.? ~> ExtractOpt) ~ (`lazy`.? ~> ExtractOpt) ~ (LocalMod.* ~> ConcatSeqNoDelim) ~> Concat4 )
    def Tmpl: R1 = rule( Prelude ~ BlockDef ~> Concat )
    def BlockStat: R1 = rule( Import | Tmpl | StatCtx.Expr )
    rule( OneOrMore(() => BlockStat, () => Semis) )
  }

  def Block: R1 = {
    def End: R1 = rule( Semis.? ~> ExtractOpt ~ &(BlockTerminator) )

    object BlockTerminatorBank extends ScopeBank[String] {
      import ScopeSwitch._

      def Normal         = rule ("}" | `case`)
      def NormalInScript = rule(
        Normal
      | "*}"  
      | "?}"  
      | "!}"  
      | ".}"  
      | "...}"
      | ":}"
      | ">>"
      | "==>"
      )

      def ruleMap = Map (
        NORMAL           -> {() => Normal}
      , NORMAL_IN_SCRIPT -> {() => NormalInScript}
      , PATTERN          -> NORMAL_IN_SCRIPT
      )
    }
    def BlockTerminator: R1 = BlockTerminatorBank().apply()

    def ResultExpr: R1 = rule{ StatCtx.Expr | LambdaHead ~ Block ~> Concat}
    def Body: R1 = rule( ResultExpr ~ End ~> Concat | BlockStats ~ ((Semis ~ ResultExpr ~> Concat).? ~> ExtractOpt) ~ End ~> Concat3 | End )
    rule( LambdaHead.* ~> ConcatSeqNoDelim ~ (Semis.? ~> ExtractOpt) ~ Body ~> Concat3 )
  }

  def Patterns: R1 = rule( OneOrMore(() => Pat, () => ",") )
  // !!! Problems might arise with pipe: whitespace before it is not likely to be permitted. In the real world, it is always almost present.
  def Pat : R1 = rule( OneOrMore(() => Pat1, () => '|') )
  def Pat1: R1 = rule( `_` ~ `:` ~ TypePat ~> Concat3 | VarId ~ `:` ~ TypePat ~> Concat3 | Pat2 )
  def Pat2: R1 = {
    def Pat3 = rule( `_*` | SimplePat ~ ((Id ~ SimplePat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( (VarId | `_`) ~ `@` ~ Pat3 ~> Concat3 | Pat3 | VarId )
  }

  def TypePat: R1 = rule( CompoundType )

  def ArgList: R1 = rule( '(' ~ ((Exprs ~ ((`:` ~ `_*` ~> Concat).? ~> ExtractOpt) ~> Concat).? ~> ExtractOpt) ~ ")" ~> Concat3 | OneNLMax ~ BlockExpr ~> Concat )

  def CaseClauseHeader: R1 = rule {`case` ~ Pat ~ (ExprCtx.Guard.? ~> ExtractOpt) ~> Concat3}

  def CaseClauses: R1 = {
    def CaseClause: R1 = rule( CaseClauseHeader ~ `=>` ~ Block ~> Concat3 )
    rule( CaseClause.+ ~> ConcatSeqNoDelim )
  }
}
