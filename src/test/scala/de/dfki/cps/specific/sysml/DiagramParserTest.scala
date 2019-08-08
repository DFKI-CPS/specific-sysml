package de.dfki.cps.specific.sysml

import java.io.File

import de.dfki.cps.specific.sysml.DiagramParser.fileToJsonString
import org.scalatest._

class DiagramParserTest  extends FlatSpec with Matchers{

  "A DiagramParser" should "correctly parse" in {
    ("diagram = " + fileToJsonString(new File("example.sysml"))) should be ("this is wrong")

  }

}
