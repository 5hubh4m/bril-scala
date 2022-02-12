# bril-scala

This is a standalone library to enable processing of [Bril](https://capra.cs.cornell.edu/bril/intro.html) programs
in Scala. It contains tools to parse a JSON representing a Bril program into a typed representation
that is easier to manipulate in a strongly typed language like Scala.

### Getting Started

This project depends upon the following dependencies to use fully:
* `java` and `sbt` for building and running
* `turnt` and `jq` for running test scripts

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

* `bril.lang.BrilAst` which contains `case class`es to represent a Bril program like `Program`, `Function`, etc.
* `bril.lang.BrilJson` which contain the logic to convert Bril programs to/from JSON.
* `bril.lang.BrilParse` contains convenience functions (`readProgramFromStdin` and `printProgramToJson`) 
  that will consume `stdin` and produce a `Try[Program]` for you and pretty print a `Program` to a JSON string.

### Testing

The library has two tests called `JsonToJsonTest` which reads a JSON Bril program and parses it to a `Program` and 
produces a pretty printed JSON string, and another call `JsonToTextTest` which takes in a JSON Bril program, parses it
to a `Program` and pretty prints it as a text Bril program. We can test the end-to-end functionality using 
[Turnt](https://github.com/cucapra/turnt) by comparing the input (unmodified) and (processed) output JSONs using some `jq` magic.

The `test` contains the all same tests used for `brili`. To use these tests, we first need to compile the package into
a fat JAR that can be run with just `java`. We use [sbt-assembly](https://github.com/sbt/sbt-assembly) for that.
From inside the project root, execute:

```
sbt assembly
```

This will build a fat JAR for the project which the script `scripts/run` can call for you with the right `mainClass`,
and is what `turnt` uses. Run the tests in the `test` directory using:

```
cd test/
turnt */*.bril
```