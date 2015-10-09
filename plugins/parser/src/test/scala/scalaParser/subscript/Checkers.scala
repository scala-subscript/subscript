package scalaParser.subscript

import scala.util._

import org.parboiled2._
import utest._

import scalaParser.Scala
import scalaParser.subscript.ast.Ast
import scalaParser.subscript.parser.MarkerParser


trait Checkers extends CheckersPlural
                  with Symbols
                  with Engine
                  with RuleCheckers {

  def checkRule[T <: Ast.Node](rl: Scala => Rule1[T], input: String, output: String) =
    checkObj(input, parse(rl, input).map(_.compile), output)

  def checkRuleStr(rl: Scala => Rule1[String], input: String, output: String) =
    checkObj(input, parse(rl, input), output)

  def check(input: String, output: String) = checkRule(_.SubScriptCodeAst, input, output)

  def checkOp(op: String, method: String) = checkRule(_.Expr9,
    s"a $op b",
    s"$method($a, $b)"
  )

  def checkBlock(symbol: String, method: String) = {
    val code = s"""val x = 3
                  |doSomethingUseful(x)
                  |2 * 2
                  |a
                  |a b c
                  |a + c
                  |x != 10
                  |x = a.b.c
                  |y?
                  |if (a == b) return 8 else println("c")
                  |9?""".stripMargin

    val rewritten =  s"""val x = $varCall("3")
                        |$varCall("doSomethingUseful($varCall(\\"x\\"))")
                        |$varCall("2 * 2")
                        |$varCall("a")
                        |$varCall("a b c")
                        |$varCall("a + c")
                        |$varCall("x != 10")
                        |$varCall("subscript.DSL._maybeVarAssignment(\\"x\\", subscript.DSL._maybeVarCall(\\"a.b.c\\"))")
                        |$varCall("y?")
                        |if ($varCall("a == b")) return $varCall("8") else $varCall("println($varCall(\\"\\\\\\"c\\\\\\"\\"))")
                        |$varCall("9?")""".stripMargin
    
    checkRule(_.CodeFragment,
      s"{$symbol\n$code\n$symbol}",
      s"""$method[Any] ($nodeVal => {
         |  implicit val $hereVal = $nodeVal
         |
         |$rewritten
         |
         |}, true)""".stripMargin
    )

    checkRule(_.CodeFragment,
      s"{${symbol}a${symbol}}"
    , s"""$method[Any] ($nodeVal => {
         |  implicit val $hereVal = $nodeVal
         |subscript.DSL._maybeVarCall("a")
         |}, true)""".stripMargin
    )
  }

  def marker(flag: Boolean)(input: String) {
    val parser = new MarkerParser(input)
    checkObj(input, parser.Identify.run(), flag)
  }

  def checkFile(name: String) {
    import scala.io.Source

    def readFile(name: String): String =
      Source.fromURL(getClass.getResource(s"/subscript/$name")).mkString

    val subscriptFile = readFile(s"${name}.scala")
    val rewrittenFile = readFile(s"${name}_rewritten.scala")

    checkRuleStr(_.CompilationUnit, subscriptFile, rewrittenFile)
  }

}

trait CheckersPlural {this: Checkers =>

  def checkOps(ops: (String, String)*) = for ((op, method) <- ops) checkOp(op, method)

  def checkMulti[T](rl: Scala => Rule1[T], test: (Scala => Rule1[T], String, String) => Unit, io: Seq[(String, String)]) =
    for ((input, output) <- io) test(rl, input, output)

  def checkRuleMulti[T <: Ast.Node](rl: Scala => Rule1[T])(io: (String, String)*) =
    checkMulti(rl, checkRule, io)

  def checkRuleStrMulti(rl: Scala => Rule1[String])(io: (String, String)*) =
    checkMulti(rl, checkRuleStr, io)

  def checkBlocks(blocks: (String, String)*) =
    for ((symbol, method) <- blocks) checkBlock(symbol, method)

}

trait Engine {
  def checkObj[T](input: String, maybeParsed: Try[T], expected: T) {
     maybeParsed match {
      case Failure(e @ ParseError(Position(_, line, column), _, _)) =>
        val lineStr   = input.split("\n")(line - 1)
        val markerStr = " " * (column - 1) + "^"

        val msg = s"""Parsing Error.
                     |--- INPUT ---
                     |$input
                     |--- LINE  ---
                     |$lineStr
                     |$markerStr
                     |--- ERROR ---
                     |$e
                     |-------------
                  """.stripMargin

        println(msg)
        throw new RuntimeException(msg)

      case Success(parsed       ) => if (parsed != expected) {
        val msg = s"""Parsing Mismatch.
                     |--- INPUT    ---
                     |$input
                     |--- PARSED   ---
                     |$parsed
                     |--- EXPECTED ---
                     |$expected
                     |----------------
                  """.stripMargin
        println(msg)
        throw new RuntimeException(msg)
      }
    }
  }

  def parse[T](rl: Scala => Rule1[T], input: String): Try[T] = {
    val parser = new Scala(input)
    parser.__run(rl(parser))
  }
}

trait RuleCheckers {this: Checkers =>
  def checkWithScript[T <: Ast.Node](rle: Scala => Rule1[T], in: String, out: String) =
    checkRule(p => p.WithScript {() => rle(p)}, in, out)

  def checkExpr9(in: String, out: String) = checkWithScript(_.Expr9, in, out)
  def checkExpr1(in: String, out: String) = checkWithScript(_.Expr1, in, out)
}