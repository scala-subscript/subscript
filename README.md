# SubScript
This is the main SubScript repository. SubScript is an extension to Scala that enables the syntax of the [Algebra of Communicating Processes](https://en.wikipedia.org/wiki/Algebra_of_Communicating_Processes).
- [Project website](http://subscript-lang.org/)
- [Syntax specification](https://github.com/scala-subscript/subscript/wiki)

## Getting Started
This section will describe how to create a simple "Hello World" application in SubScript from scratch.
To get started using SubScript:

1. Create a new SBT project
  1. Create a new directory which will be the root of the project. Hereafter, we'll assume it's name is `root`: `mkdir root`
  2. Create the directory for the sources: `mkdir -pv src/main/scala`
  3. Create the build file: `touch build.sbt`
  4. Create the project configuration directory: `mkdir project`
  5. Create the project configuration build file: `touch project/build.sbt`
  6. Create the main source file for your project: `touch src/main/scala/Main.scala`
  7. Download and install SBT itself, if you haven't done it already, using the [manual](http://www.scala-sbt.org/download.html) on the [official website](http://www.scala-sbt.org/)
2. In `project/build.sbt`, write the following code:
  
  ```scala
  addSbtPlugin("org.subscript-lang" %% "subscript-sbt-plugin" % "1.0.0")
  ```
  It adds the SubScript plugin to the SBT build tool, so that it can understand SubScript sources.
3. In `build.sbt`, write the following code:
  
  ```scala
  scalaVersion := "2.11.7"
  libraryDependencies += "org.subscript-lang" %% "subscript-swing" % "2.0.0"
  SubscriptSbt.projectSettings
  ```
  First line sets the Scala version to be used, second sets a dependency on `subscript-swing` and third applies the SubScript SBT plugin.
  Note: you can declare a dependency on `"org.subscript-lang" %% "subscript-core" % "2.0.0"` instead of `subscript-swing`, but you need `subscript-swing` to be able to use the debugger.
4. In `src/main/scala/Main.scala`, write the following code:

  ```scala
  import subscript.language

  object Main {
    def main(args: Array[String]): Unit = subscript.DSL._execute(live)

    script live = {!println("Hello")!} {!println("World")!}
  }
  ```
  Here, `import subscript.language` enables SubScript syntax in this file. Each file that needs to use SubScript syntax must have this top-level import.
  `subscript.DSL._execute(live)` calls a core SubScript method that executes the script provided as an argument.
  Finally, `script live = {!println("Hello")!} {!println("World")!}` is a simple script that prints "Hello World" separated by a new line from two Scala code blocks.