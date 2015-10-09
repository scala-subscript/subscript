# SubScript
This is the main SubScript repository. SubScript is an extension to Scala that enables the syntax of the [Algebra of Communicating Processes](https://en.wikipedia.org/wiki/Algebra_of_Communicating_Processes).
- [Project website](http://subscript-lang.org/)
- [Syntax specification](https://github.com/scala-subscript/subscript/wiki)

## Getting Started
To get started using SubScript in your project:

1. Create a new ordinary SBT project
2. In `project/build.sbt` (create if not exists), write the following code:
  
  ```scala
  addSbtPlugin("org.subscript-lang" %% "subscript-sbt-plugin" % "1.0.0")
  ```
3. In `build.sbt`, write the following code:
  
  ```scala
  scalaVersion := "2.11.7"
  libraryDependencies += "org.subscript-lang" %% "subscript-core" % "2.0.0"
  SubscriptSbt.projectSettings
  ```
4. The sources that need to use SubScript syntax must contain the top-level import:
  ```scala
  import subscript.file
  ```
  You can execute scripts in the sources with `subscript.DSL._execute(scriptName)`.
  For example, create src/main/scala/Main.scala with the following content:
  ```scala
  import subscript.file

  object Main {
    def main(args: Array[String]): Unit = subscript.DSL._execute(live)

    script live = {!println("Hello")!} {!println("World")!}
  }
  ```
