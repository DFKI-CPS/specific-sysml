package de.dfki.cps.specific.sysml.parser

import scala.util.parsing.input.Position

sealed trait Severity
object Severity {
  case object Error extends Severity
  case object Warn extends Severity
}

case class ParseError(source: String, position: Position, severity: Severity, message: String)
