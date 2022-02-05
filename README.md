# bril-scala

This is a standalone library to enable processing of [Bril](https://capra.cs.cornell.edu/bril/intro.html) programs
in Scala. It contains tools to parse a JSON representing a Bril program into a typed representation
that is easier to manipulate in a strongly typed language like Scala.

### Getting Started

You can compile the library and publish it to your local [Ivy](https://ant.apache.org/ivy/) repository using `sbt`.
If you have `sbt` installed, from inside the project root, execute:

```
sbt publishLocal
```

This will publish a `jar` to a local repository.

### Using in a Project

To use the locally published `jar` in an `sbt` project just add the following lines
in your `build.sbt` file for the project:

```
resolvers += "Lcl" at f"file://${System.getProperty("user.home")}/.ivy2/local/default",
libraryDependencies += "edu.cornell" %%  "bril-scala" % "0.1.0"
```

### Overview

The library has three main classes:

* `bril.lang.BrilAST` which contains `case class`es to represent a Bril program like `Program`, `Function`, etc.
* `bril.lang.BrilJson` which contain the logic to convert Bril programs to/from JSON/
* `bril.lang.BrilParse` contains convenience functions (`readProgramFromStdin` and `printProgramToJson`) 
  that will consume `stdin` and produce a `Try[Program]` for you and pretty print a `Program` to a JSON string.