# SubScript
This is the main SubScript repository. SubScript is an extension to Scala that enables the syntax of the [Algebra of Communicating Processes](https://en.wikipedia.org/wiki/Algebra_of_Communicating_Processes). This is a place to get started using SubScript, for more theoretical foundation see [project website](http://subscript-lang.org/).

## Prerequisites
In order to get started using SubScript, one should have an [SBT](http://www.scala-sbt.org/) build tool installed (use the official [installation guide](http://www.scala-sbt.org/download.html)) and know the basics of working with the command line.

You should also have Java 8.

**Note for Mac OS users:** if you try to install SBT with [homebrew](http://brew.sh/), you may encounter the following error:
```
Cowardly refusing to `sudo brew install'
```
This is due to [this](https://github.com/Homebrew/homebrew/issues/9953) issue, you can solve this by running the following command:
```
sudo chown root /usr/bin/brew
``` 

## Getting Started
This section will describe how to create a simple "Hello World" application in SubScript from scratch.

1. Create a new SBT project, run the following commands:

  ```bash
  mkdir subscript-hello-world
  cd subscript-hello-world
  mkdir -pv src/main/scala
  touch build.sbt
  mkdir project
  touch project/build.sbt
  touch src/main/scala/Main.scala
  ```
  Here is a detailed description of what each of them do:
  1. Create a new directory which will be the root of the project. Hereafter, we'll assume it's name is `subscript-hello-world`: `mkdir subscript-hello-world`
  2. Cd to this directory: `cd subscript-hello-world`
  3. Create the directory for the sources: `mkdir -pv src/main/scala`
  4. Create the build file: `touch build.sbt`
  5. Create the project configuration directory: `mkdir project`
  6. Create the project configuration build file: `touch project/build.sbt`
  7. Create the main source file for your project: `touch src/main/scala/Main.scala`
2. In `project/build.sbt`, write the following code:
  
  ```scala
  addSbtPlugin("org.subscript-lang" %% "subscript-sbt-plugin" % "3.0.1")
  ```
  It adds the SubScript plugin to the SBT build tool, so that it can understand SubScript sources.
3. In `build.sbt`, write the following code:
  
  ```scala
  scalaVersion := "2.11.7"
  libraryDependencies += "org.subscript-lang" %% "subscript-swing" % "3.0.1"
  SubscriptSbt.projectSettings
  ```
  First line sets the Scala version to be used, second sets a dependency on `subscript-swing` and third applies the SubScript SBT plugin.
  Note: you can declare a dependency on `"org.subscript-lang" %% "subscript-core" % "3.0.1"` instead of `subscript-swing`, but you need `subscript-swing` to be able to use the debugger.
4. In `src/main/scala/Main.scala`, write the following code:

  ```scala
  import subscript.language
  import subscript.Predef._

  object Main {
    def main(args: Array[String]): Unit = runScript(live)

    script live = {!println("Hello")!} {!println("World")!}
  }
  ```
  Here, `import subscript.language` enables SubScript syntax in this file. Each file that needs to use SubScript syntax must have this top-level import.
  `import subscript.Predef._` imports predefined functions to simplify work with scripts, such as `runScript`.
  `runScript(live)` calls a core SubScript method that executes the script provided as an argument.
  Finally, `script live = {!println("Hello")!} {!println("World")!}` is a simple script that prints "Hello World" from two Scala code blocks.
5. Execute the project by running `sbt run`
6. Debug the project with SubScript Graphical Debugger by running `sbt ssDebug`


## Further learning
- [Koans](https://github.com/scala-subscript/koans) is an easy way to learn the fundamentals of SubScript by doing.
- To check out the official examples, go to the [examples repository](https://github.com/scala-subscript/examples) and follow its "Getting Started" guide.
- [Cheat sheet](http://www.cheatography.com/anatoliykmetyuk/cheat-sheets/subscript/) will help you to remember the language.

## Getting Started for Contributors
### Structure
This repository consists of two SBT projects under correspondent directories:
- **plugins** with following subprojects
  - **parser** - a parser capable of parsing SubScript-containing Scala code
  - **enhanced-macros** - a compiler plugin to tweak macros to enable certain SubScript-specific functionality. It affects only specific SubScript methods, so it won't break your macro-containing code!
  - **plugin-parser** - an SBT plugin that uses the *parser* to generate synthetic pure-Scala sources from SubScript-containing ones. Also, it applies *enhanced-macros* compiler plugin.
- **framework** with following subprojects
  - **core** - core SubScript library, contains all the essential functionality
  - **swing** - enhancements to the Scala Swing library. Contains SubScript Graphical Debugger
  - **akka** - integration layer with Akka

**IMPORTANT:** `framework` projects depend on `plugins` projects. Before building `framework`, make sure you've pushed `framework` to your local Ivy2 repository (see below).

### Compilation procedure
In a correctly configured SubScript project (see Getting Started guide to see what it is), the following happens when you run `sbt compile`:

1. SubScript *parser plugin* (resides under `plugins/plugin-parser`) uses the *parser* (`plugins/parser`) to process all the files that have `import subscript.language` top-level import.
2. Pure-Scala synthetic sources are generated as a result of step (1). They go under `target/scala-2.11/src_managed`. The original sources processed during (1) are ignored by the compiler hereafter.
3. The Scala compiler compiles all the sources under `src/`, except those with `import subscript.language`, and all the synthetic sources under `target/scala-2.11/src_managed`.

### Synthetic sources
Scala parser generates synthetic sources from SubScript-containing ones. The idea is that SubScript syntax is mapped to the methods from `subscript.DSL` (see *core* project). For example, the following SubScript code:

```scala
  script live = {!println("Hello")!} {!println("World")!}
```

will be transformed into:

```scala
def live = subscript.DSL._script[Any](None, Symbol("live")){(_node: subscript.vm.Script[Any]) =>
  implicit val script = _node
subscript.DSL._seq(subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("println(subscript.DSL._maybeVarCall(\"\\\"Hello\\\"\"))")
}, true), subscript.DSL._normal[Any] (_node => {
  implicit val here = _node
subscript.DSL._maybeVarCall("println(subscript.DSL._maybeVarCall(\"\\\"World\\\"\"))")
}, true))}
}
```

### Development procedure
1. Fork this repository and clone your fork to your computer
2. `cd` to the root of your fork
3. Do the necessary changes to the proper projects
4. Clear your local Ivy2 repository with the following command:
  ```
  find ~/.ivy2/ -type d -name "*subscript*" -print0 | xargs -0 rm -rf
  ```
  It will search for directories containing "subscript" in their name and located under ~/.ivy2, then it will feed all them to the `rm` command.

5. Publish the `plugins` and the `framework` to the local repository with the following two commands:
  ```
  (cd plugins/; sbt publish-local)
  (cd framework/; sbt publish-local)
  ```
6. Test your changes using any SubScript project on your machine
7. Commit your changes to your fork
8. Send us the pull request with your changes

### Versioning
The SubScript plugins (the preprocessor and Scalac macros) and the framework (VM, debugger, the bridges to Swing and Akka) are versioned using the [semantic versioning](http://semver.org/) model. Public release versions are composed of three digits (`1.2.3`). The `build.sbt` files mention these versions.

When you are making local changes to the plugins or framework, you should use version names ending in `-SNAPSHOT`, e.g., `1.2.3-SNAPSHOT`.
Then you can test these changes against the other projects such as Examples. Those projects should refer to the correct version of the SubScript plugins and framework, as a dependency in their `build.sbt` and `project/build.sbt` files.

In order to change the SubScript version, run the command line script `./set_version.sh new_version` from the root of the project.
For example: `./set_version.sh 3.0.0-SNAPSHOT`.

## TL;DR Simply install everything
 * Open a command line terminal
 * Create a workspace directory, and make it the current directory
 * Perform the following commands:
```
      git clone https://github.com/scala-subscript/subscript
      git clone https://github.com/scala-subscript/eye-test
      git clone https://github.com/scala-subscript/koans
      git clone https://github.com/scala-subscript/examples
```     
 * In the IntelliJ IDE you may import the entire directory tree; handy for file viewing and editing
 * Compile and run outside the IDE on the command line using the instructions for the projects

