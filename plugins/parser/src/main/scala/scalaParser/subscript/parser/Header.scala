package scalaParser.subscript.parser

import language.implicitConversions
import org.parboiled2._
import scalaParser._
import scalaParser.syntax._

import scalaParser.subscript.ast.Ast


trait Header {this: SubScript with Exprs =>

  def ScriptHeader: R[Ast.ScriptHeader] = {
    def AnnotBlahBlah: R1 = rule( Annot.* ~> ConcatSeqNoDelim ~ TypeArg ~> Concat )
    def FunTypeArgs  : R1 = rule( '[' ~ OneOrMore(() => AnnotBlahBlah, () => ',') ~ ']' ~> Concat3 )
    
    def FunAllArgs: R[(Seq[Seq[Ast.Parameter]], Option[Seq[Ast.Parameter]])] = {
      def Trans1: (String, String, String, Seq[Ast.Parameter], String) => Seq[Ast.Parameter] =
        (_, _, _, args, _) => args

      def Trans2: (Seq[Seq[Ast.Parameter]], Option[Seq[Ast.Parameter]]) => (Seq[Seq[Ast.Parameter]], Option[Seq[Ast.Parameter]]) =  // Create a pair
        (a, b) => (a, b)

      rule( FunArgs.* ~ (OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')' ~> Trans1).? ~> Trans2 )
    }
    
    def FunArgs: R[Seq[Ast.Parameter]] = {
      def Trans1: (String, String, Option[Seq[Ast.Parameter]], String) => Seq[Ast.Parameter] = (_, _, args, _) => args.getOrElse(Nil)
      rule( OneNLMax ~ '(' ~ Args.? ~ ')' ~> Trans1 )
    }

    def Args: R[Seq[Ast.Parameter]] = {
      def FunArgGen[A <: Ast.Parameter](id: () => R1, argFun: (String, String, String, Option[String]) => A): R[A] = rule {
        Annot.* ~> ConcatSeqNoDelim ~ id() ~ (`:` ~ Spaces(() => ParamType) ~> SecondStr) ~ (`=` ~ Spaces(() => TypeExpr) ~> SecondStr).? ~> argFun
      }

      def  QId: R1 = rule { WSR0 ~ '?'  ~ Id ~> SecondStr }
      def QQId: R1 = rule { WSR0 ~ "??" ~ Id ~> SecondStr }

      def NormalParameter           : R[Ast.NormalParameter           ] = FunArgGen(() =>   IdS, Ast.           NormalParameter)
      def FormalOutputParameter     : R[Ast.FormalOutputParameter     ] = FunArgGen(() =>  QId , Ast.     FormalOutputParameter)
      def FormalConstrainedParameter: R[Ast.FormalConstrainedParameter] = FunArgGen(() => QQId , Ast.FormalConstrainedParameter)

      def FunArg: R[Ast.Parameter] = rule (
        FormalConstrainedParameter
      | FormalOutputParameter
      | NormalParameter
      )
      
      rule( FunArg + ',' )
    }


    rule( (!SSKeyword ~ Id ~ (FunTypeArgs.? ~> ExtractOpt) ~> Concat) ~ FunAllArgs ~ (`:` ~ Spaces(() => Type) ~> SecondStr).? ~>
      {(id: String, args: (Seq[Seq[Ast.Parameter]], Option[Seq[Ast.Parameter]]), tpe: Option[String]) => // Seq[Seq[Arg]] - ordinary Parameters; Option[Seq[Arg]] - implicit Parameters
        val (explicits, implicits) = args
        Ast.ScriptHeader(id, explicits, implicits, tpe)
      }
    )
  }

}