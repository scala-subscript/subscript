package scalaParser

import org.parboiled2._

import shapeless._

trait RulesOps
  extends StringRulesOps
  with    Metarules
  {this: Parser =>}

trait StringRulesOps extends StringTransformFunctions {this: Parser with Metarules =>

  def OneOrMore(term: () => Rule1[String], delim: () => Rule1[String]): Rule1[String] = {
    def Factory: ((String, Seq[(String, String)])) => String = {case (head, tail) =>
      head + tail.map {case (a, b) => a + b}.mkString
    }

    rule {OneOrMoreSeq(term, delim) ~> Factory}
  }

  def ZeroOrMore(term: () => Rule1[String], delim: () => Rule1[String]): Rule1[String] = rule {
    OneOrMore(term, delim).? ~> ExtractOpt
  }

}

trait StringTransformFunctions {
  type S = String

  def Concat : (S, S)                => S = _+_
  def Concat3: (S, S, S)             => S = _+_+_
  def Concat4: (S, S, S, S)          => S = _+_+_+_
  def Concat5: (S, S, S, S, S)       => S = _+_+_+_+_
  def Concat6: (S, S, S, S, S, S)    => S = _+_+_+_+_+_
  def Concat7: (S, S, S, S, S, S, S) => S = _+_+_+_+_+_+_

  def ConcatSeq(delimiter: String): (Seq[String]) => String = _.mkString(delimiter)

  def ConcatSeqSemi    = ConcatSeq(";")
  def ConcatSeqNoDelim = ConcatSeq("" )
  def ConcatSeqComma   = ConcatSeq(",")
  def ConcatSeqDot     = ConcatSeq(".")
  def ConcatSeqWith    = ConcatSeq(" with ")
  def ConcatSeqPipe    = ConcatSeq("|")

  def ExtractOpt: Option[String] => String = _.getOrElse("")

  def SecondStr: (S, S) => S = (a: S, b: S) => b
}

trait Metarules {this: Parser =>
  
  def OneOrMoreSeq[A, B](term: () => Rule1[A], delim: () => Rule1[B]): Rule1[(A, Seq[(B, A)])] =
    rule ( term() ~ (delim() ~ term() ~> {(b: B, a: A) => b -> a}).* ~> {(a: A, others: Seq[(B, A)]) => a -> others} )

  def ZeroOrMoreSeq[A, B](term: () => Rule1[A], delim: () => Rule1[B]): Rule1[Option[(A, Seq[(B, A)])]] =
    rule ( OneOrMoreSeq(term, delim).? )

  def Code(code: => Unit): Rule0 = {code; MATCH}

  def Try[T](rle: () => Rule1[T], before: => Unit, after: => Unit = (), rollback: => Unit = ()): Rule1[T] = rule (
    Code(before  ) ~ rle() ~ Code(after)
  | Code(rollback) ~ MISMATCH[HNil, T :: HNil]
  )

}