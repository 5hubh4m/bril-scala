package bril.lang

import bril.lang.BrilAST._
import bril.lang.BrilJson._
import spray.json._

import scala.io.Source
import scala.util._

/**
 * Utilities for parsing and printing Bril
 * programs in JSON.
 */
case object BrilParse {

  /**
   * Parse a Bril program from stdin.
   */
  def readProgramFromStdin: Try[Program] =
    Try(Source.stdin.iter.foldLeft("")({ case str -> c => str :+ c }).parseJson.convertTo[Program])

  /**
   * Pretty print the JSON of a Bril program.
   */
  def printProgramToJson(program: Program): String = program.toJson.prettyPrint

}
