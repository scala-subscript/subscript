package scalaParser

trait Types extends Core{

  def TypeExpr: R1

  private implicit def wspStr(s: String): R1 = rule( WL ~ capture(str(s)) ~> Concat )
  private implicit def wspCh (s: Char  ): R1 = rule( WL ~ capture(ch (s)) ~> Concat )

  def Mod: R1 = rule( LocalMod | AccessMod | `override` )
  def LocalMod: R1 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessMod: R1 = {
    def AccessQualifier: R1 = rule( '[' ~ (`this` | Id) ~ ']' ~> Concat3 )
    rule( (`private` | `protected`) ~ (AccessQualifier.? ~> ExtractOpt) ~> Concat )
  }

  def Dcl: R1 = {
    def VarDcl: R1 = rule( `var` ~ Ids ~ `:` ~ Type ~> Concat4 )
    def FunDcl: R1 = rule( `def` ~ FunSig ~ ((`:` ~ Type ~> Concat).? ~> ExtractOpt) ~> Concat3 )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }

  def Type: R1 = {
    def FunctionArgTypes: R1 = rule('(' ~ (OneOrMore(() => ParamType, () => ',').? ~> ExtractOpt) ~ ')' ~> Concat3 )
    def ArrowType: R1 = rule( FunctionArgTypes ~ `=>` ~ Type ~> Concat3 )
    def TypeOrValDcl: R1 = rule(TypeDcl | ValDcl)
    def ExistentialClause: R1 = rule( `forSome` ~ `{` ~ OneOrMore(() => TypeOrValDcl, () => Semis) ~ `}` ~> Concat4 )
    def PostfixType: R1 = rule( InfixType ~ (`=>` ~ Type ~> Concat | ExistentialClause.? ~> ExtractOpt) ~> Concat )
    def Unbounded: R1 = rule( `_` | ArrowType | PostfixType )
    rule( Unbounded ~ TypeBounds ~> Concat )
  }

  def InfixType: R1 = rule( CompoundType ~ ((NotNewline ~ Id ~ OneNLMax ~ CompoundType ~> Concat4).* ~> ConcatSeqNoDelim) ~> Concat )

  def CompoundType: R1 = {
    def RefineStat: R1 = rule( TypeDef | Dcl  )
    def Refinement: R1 = rule( OneNLMax ~ `{` ~ ZeroOrMore(() => RefineStat, () => Semis) ~ `}` ~> Concat4 )
    rule( OneOrMore(() => AnnotType, () => `with`) ~ (Refinement.? ~> ExtractOpt) ~> Concat | Refinement )
  }
  def AnnotType: R1 = rule(SimpleType ~ ((NotNewline ~ ((NotNewline ~ Annot ~> Concat).+ ~> ConcatSeqNoDelim) ~> Concat).? ~> ExtractOpt) ~> Concat )

  def SimpleType: R1 = {
    def BasicType: R1 = rule( '(' ~ Types ~ ')' ~> Concat3 | StableId ~ '.' ~ `type` ~> Concat3 | StableId )
    rule( BasicType ~ ((TypeArgs | `#` ~ Id ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
  }

  def TypeArgs: R1 = rule( '[' ~ Types ~ "]" ~> Concat3 )
  def Types: R1 = rule( OneOrMore(() => Type, () => ',') )

  def ValDcl : R1 = rule( `val` ~ Ids ~ `:` ~ Type ~> Concat4 )
  def TypeDcl: R1 = rule( `type` ~ Id ~ (TypeArgList.? ~> ExtractOpt) ~ TypeBounds ~> Concat4 )

  def FunSig: R1 = {
    def AnnotBlahBlah: R1 = rule( Annot.* ~> ConcatSeqNoDelim ~ TypeArg ~> Concat )
    def FunTypeArgs: R1 = rule( '[' ~ OneOrMore(() => AnnotBlahBlah, () => ',') ~ ']' ~> Concat3 )
    def FunAllArgs: R1  = rule( FunArgs.* ~> ConcatSeqNoDelim ~ ((OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')' ~> Concat5).? ~> ExtractOpt) ~> Concat )
    def FunArgs: R1     = rule( OneNLMax ~ '(' ~ (Args.? ~> ExtractOpt) ~ ')' ~> Concat4 )
    def FunArg: R1      = rule( Annot.* ~> ConcatSeqNoDelim ~ Id ~ ((`:` ~ ParamType ~> Concat).? ~> ExtractOpt) ~ ((`=` ~ TypeExpr ~> Concat).? ~> ExtractOpt) ~> Concat4 )
    def Args: R1        = rule( OneOrMore(() => FunArg, () => ',') )
    rule( (Id | `this`) ~ (FunTypeArgs.? ~> ExtractOpt) ~ FunAllArgs ~> Concat3 )
  }
  def ParamType: R1 = rule( `=>` ~ Type ~> Concat | Type ~ "*" ~> Concat | Type )

  def TypeBounds: R1 = rule( (`>:` ~ Type ~> Concat).? ~> ExtractOpt ~ ((`<:` ~ Type ~> Concat).? ~> ExtractOpt) ~> Concat )
  def TypeArg: R1 = {
    def CtxBounds: R1 = rule((`<%` ~ Type ~> Concat).* ~> ConcatSeqNoDelim ~ ((`:` ~ Type ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat)
    rule((Id | `_`) ~ (TypeArgList.? ~> ExtractOpt) ~ TypeBounds ~ CtxBounds ~> Concat4)
  }

  def Annot: R1 = {
    def SugaredExprsBody: R1 = rule{ (Exprs ~ ((`:` ~ `_*` ~> Concat).? ~> ExtractOpt) ~> Concat).? ~> ExtractOpt }
    def SugaredExprs: R1     = rule{ ('(' ~ SugaredExprsBody ~ ")" ~> Concat3).* ~> ConcatSeqNoDelim }
    rule( `@` ~ SimpleType ~ SugaredExprs ~> Concat3 )
  }

  def TypeArgList: R1 = {
    def Variant: R1 = rule( Annot.* ~> ConcatSeqNoDelim ~ (capture(WLR0 ~ anyOf("+-")).? ~> ExtractOpt) ~ TypeArg ~> Concat3 )
    rule( '[' ~ ZeroOrMore(() => Variant, () => ',') ~ ']' ~> Concat3 )
  }
  def Exprs: R1 = rule( OneOrMore(() => TypeExpr, () => ',') )
  def TypeDef: R1 = rule( `type` ~ Id ~ (TypeArgList.? ~> ExtractOpt) ~ `=` ~ Type ~> Concat5 )
}
