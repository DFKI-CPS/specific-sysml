package de.dfki.cps.specific.sysml

import java.io.File
import java.util

import de.dfki.cps.specific.SysML
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import scala.collection.mutable
import scala.util.parsing.input.Position

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  implicit val rs = new ResourceSetImpl
  Synthesis.prepareLibrary(rs)

  println("loading project")
  val projectURI = URI.createFileURI(s"example/index.sysml")

  val resource = rs.createResource(projectURI.appendFileExtension("ecore"))

  val positions = mutable.Map.empty[EObject,(String,Position)]

  def load = (uri: URI) => {
    println(s"loading $uri")
    val resource = rs.createResource(uri.appendFileExtension("ecore"))
    val positions = SysML.load(new File(uri.toFileString),resource)
    resource.save(new util.HashMap)
    resource
  }

  SysML.loadProject(projectURI, resource, load)

  println("saving")

  resource.save(new util.HashMap)

  println("done.")
}
