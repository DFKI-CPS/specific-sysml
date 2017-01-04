package de.dfki.cps.specific.sysml

/**
  * Created by martin on 1/4/17.
  */
object indent {
  def apply(lines: String): String = lines.lines.map("  " + _).mkString("\n")
}
