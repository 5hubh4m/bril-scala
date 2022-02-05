package bril.test

import bril.lang.BrilParse._

/**
 * This test takes in a JSON Bril program,
 * parses it into an AST, and then produces
 * a JSON back.
 */
case object JsonToJsonTest extends App {

  // read the program from stdin and print the JSON back to stdout
  print(printProgramToJSON(readProgramFromStdin.get))

}
