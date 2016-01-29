lazy val commonSettings = Seq(
  organization       := "org.subscript-lang"
, version            := "3.0.1-SNAPSHOT"
  
, publishTo := {
    if (isSnapshot.value)
      Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/") 
    else
      Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  }

, scalaVersion := "2.10.4"

, pomExtra :=
    <url>https://github.com/scala-subscript/subscript</url>
    <licenses>
      <license>
        <name>GNU GPL</name>
        <url>http://www.gnu.org/copyleft/gpl.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git://github.com/scala-subscript/subscript.git</url>
      <connection>scm:git://github.com/scala-subscript/subscript.git</connection>
    </scm>
    <developers>
      <developer>
        <id>anatoliykmetyuk</id>
        <name>Anatoliy Kmetyuk</name>
        <url>https://github.com/anatoliykmetyuk</url>
      </developer>
    </developers>
)

lazy val root = (project in file ("."))
  .aggregate(parser, parserPlugin, enhancedMacros)
  .settings(
    packagedArtifacts := Map.empty  // Don't publish root to maven
  )

lazy val parser = (project in file("parser"))
  .settings(commonSettings)
  .settings(
    name := "subscript-parser"
  , crossScalaVersions := Seq("2.11.4", "2.10.5")
    
  , libraryDependencies ++= Seq(
      "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
      "com.lihaoyi" %% "utest" % "0.3.0",
      "org.parboiled" %% "parboiled" % "2.1.0",
      "org.scalatest" %% "scalatest" % "2.2.4"
    )

  , addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")
  , autoCompilerPlugins := true

  , resolvers      += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  , testFrameworks += new TestFramework("utest.runner.Framework")
  
  , initialCommands := """
      |import util._
      |
      |import scalaParser._
      |import syntax._
      |import org.parboiled2._
      |
      |import scalaParser.subscript._
      |import ast.Ast._
      |
      |def p (in: String) = new Scala(in).CompilationUnit.run()
      |def ss(in: String) = new Scala(in).SubScriptCode.run()
      |def sp[T](p: Scala => Rule1[T], in: String): Try[T] = {
      |  val parser = new Scala(in)
      |  parser.__run(p(parser))
      |}
    """.stripMargin

  , excludeFilter in Test := "OperatorsSuite.scala" | "TermsSuite.scala" | "IntegratedSuite.scala"
  )

lazy val parserPlugin = (project in file("plugin-parser"))
  .dependsOn(parser)
  .settings(commonSettings)
  .settings(
    name := "subscript-sbt-plugin"
  , sbtPlugin := true
  )

lazy val enhancedMacros = (project in file("enhanced-macros"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.11.7"
  , name         := "enhancedmacros"

  , libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.11.7"
    , "org.scala-lang" % "scala-reflect" % "2.11.7"
    )
  )
