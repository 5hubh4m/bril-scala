package bril.test

import bril.lang.BrilParse._

/**
 * This test takes in a JSON Bril program,
 * parses it into an AST, and then produces
 * a JSON back.
 */
object JsonToTextTest extends App {

  // read the program from stdin and print the pretty printed output back to stdout
  print(readProgramFromStdin.get.prettyPrint)

}
