package subscript.plugin

import scala.util._

import sbt.Keys._
import sbt._

import org.parboiled2._

import scalaParser._
import scalaParser.subscript.parser.MarkerParser

import sbt.Attributed.data

object SubscriptSbt extends sbt.AutoPlugin with SubscriptSbtHelpers {

  def isSubscript(f: File): Boolean = if (f.exists && f.isFile) new MarkerParser(readFile(f)).Identify.run() match {
    case Success(flag: Boolean) => flag
    case Failure(e: ParseError) => parseError(e, f)
  }
  else false

  val subscriptFilter = new SimpleFileFilter(isSubscript)

  val mySeq = Seq(
    managedSources ++= (scalaSource.value ** "*.scala").get
      .filter(isSubscript)
      .map {in =>
        val outName = managedSourceDirectories.value.head.getAbsolutePath +
          in.getAbsolutePath.drop(scalaSource.value.getAbsolutePath.length)
        val out = new sbt.File(outName)

        val src = readFile(in)

        val settings = new MarkerParser(src).ExtractImports.run() match {
          case Success(imports) =>
            val pattern = """\s*import\s+subscript\.file\.parserSettings\.(.+)""".r
            imports.filter {pattern.findFirstIn(_).isDefined}.map {case pattern(setting) => setting}
          case Failure(_) => Nil
        }

        new Scala(readFile(in), settings).CompilationUnit.run() match {
          case Success(code         ) => IO.write(out, code)
          case Failure(e: ParseError) => parseError(e, in  )
        }

        out
      }

  , excludeFilter in unmanagedSources := (excludeFilter in unmanagedSources).value || subscriptFilter
  )

  lazy val ssDebug     = taskKey[Unit]("Debug the main SubScript script with a debugger written in SubScript" )
  lazy val ssDebugPure = taskKey[Unit]("Debug the main SubScript script with a debugger written in pure Scala")

  override val projectSettings = inConfig(Test)(mySeq) ++ inConfig(Compile)(mySeq) ++ Seq(
    libraryDependencies ++= Seq(
    //   "org.subscript-lang" % "subscript" % "1.0"
    // , "org.subscript-lang" % "subscript-swing" % "1.0"
    // , "org.scala-lang" % "scala-swing" % "2.11.0-M7"
    )

  , addCompilerPlugin("org.subscript-lang" %% "enhancedmacros" % "3.0.4")

  , ssDebug := {
      val r  = (runner        in Compile).value
      val mc = (mainClass     in Compile).value.get
      val cp = (fullClasspath in Compile).value
      toError(r.run("subscript.swing.SubScriptDebugger", data(cp), Seq(mc), streams.value.log))
    }

  , ssDebugPure := {
      val r  = (runner        in Compile).value
      val mc = (mainClass     in Compile).value.get
      val cp = (fullClasspath in Compile).value
      toError(r.run("subscript.swing.PureScalaDebugger", data(cp), Seq(mc), streams.value.log))
    }
  )
}

trait SubscriptSbtHelpers {this: AutoPlugin =>
  def readFile(f: File): String = io.Source.fromFile(f.getAbsolutePath).mkString

  def parseError(e: ParseError, file: File): Nothing = e match {case ParseError(Position(_, line, column), _, _) => 
    val lineStr   = readFile(file).split("\n")(line - 1)
    val markerStr = " " * (column - 1) + "^"
    val filePath  = file.getAbsolutePath

    val error = s"Parse error\nFile: $filePath\nError: $e\n$lineStr\n$markerStr"
    toError(Some(error)).asInstanceOf[Nothing]
  }
}
