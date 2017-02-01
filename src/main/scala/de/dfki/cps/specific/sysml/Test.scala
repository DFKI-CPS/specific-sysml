package de.dfki.cps.specific.sysml

import java.io.File
import java.util

import de.dfki.cps.specific.SysML
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.impl.{ExtensibleURIConverterImpl, ResourceSetImpl}
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import scala.util.parsing.input.Reader
import scala.collection.JavaConverters._

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  implicit val rs = new ResourceSetImpl
  Synthesis.prepareLibrary(rs)
  val resources = Seq(
    "index"
  )
  resources.foreach { name =>
    val uri = URI.createFileURI(s"example/$name.sysml")
    val resource = rs.createResource(uri.appendFileExtension("uml"))
    val positions = SysML.load(new File(uri.toFileString),resource)
    resource.save(new util.HashMap)
  }
}
