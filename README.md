## bril-scala

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

The library has two classes `bril.lang.BrilAST` which contains `case class`es to represent a
Bril program like `Program`, `Function`, etc.

The class `bril.lang.BrilParse` contains functions that take in a JSON input and convert it into
a `Program`. There is even a convenience function (called `readProgramFromStdin`) that will parse
the `stdin` and produce a `Option[Program]` for you.