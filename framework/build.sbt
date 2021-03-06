lazy val commonSettings = Seq(
  organization       := "org.subscript-lang"
, version            := "3.0.5-SNAPSHOT"
  
, publishTo := {
    if (isSnapshot.value)
      Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/") 
    else
      Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  }

, scalaVersion := "2.11.7"

, pomExtra :=
    <url>https://github.com/scala-subscript/subscript</url>
    <licenses>
      <license>
        <name>GNU LGPL</name>
        <url>http://www.gnu.org/licenses/lgpl-3.0.en.html</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git://github.com/scala-subscript/subscript.git</url>
      <connection>scm:git://github.com/scala-subscript/subscript.git</connection>
    </scm>
    <developers>
      <developer>
        <name>Andre van Delft</name>
        <url>https://github.com/AndreVanDelft</url>
      </developer>
      <developer>
        <id>anatoliykmetyuk</id>
        <name>Anatoliy Kmetyuk</name>
        <url>https://github.com/anatoliykmetyuk</url>
      </developer>
    </developers>
) ++ SubscriptSbt.projectSettings

lazy val root = (project in file("."))
  .aggregate(coreJVM, coreJS, akka, swing, corescripts)
  .settings(
    packagedArtifacts := Map.empty  // Don't publish root to maven
  )

lazy val core = (crossProject in file("core"))
  .settings(commonSettings)
  .settings(
    name    := "subscript-core"
  , libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"
  // , excludeFilter in (Test, unmanagedSources) := "*.scala"
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val akka = (project in file("akka"))
  .dependsOn(coreJVM, corescripts)
  .settings(commonSettings)
  .settings(
    name := "subscript-akka"
  , libraryDependencies += "com.typesafe.akka"  %% "akka-actor" % "2.3.11"
  )

lazy val swing = (project in file("swing"))
  .dependsOn(coreJVM, corescripts)
  .settings(commonSettings)
  .settings(
    name := "subscript-swing"
  , libraryDependencies += "org.scala-lang"     %  "scala-swing"    % "2.11.0-M7"
  , excludeFilter in Compile := "Scripts.scala" | "SubScriptDebugger.scala"
  )

lazy val corescripts = (project in file("core-scripts"))
  .dependsOn(coreJVM)
  .settings(commonSettings)
  .settings(
    name := "subscript-core-scripts"
  , libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )
