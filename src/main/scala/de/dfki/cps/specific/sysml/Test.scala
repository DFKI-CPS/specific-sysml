package de.dfki.cps.specific.sysml

import java.io.File
import java.util

import de.dfki.cps.specific.SysML
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import org.eclipse.uml2.uml.{Model, UMLFactory}

import scala.collection.JavaConverters._

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  implicit val rs = new ResourceSetImpl
  Synthesis.prepareLibrary(rs)

  println("loading project")
  val projectURI = URI.createFileURI(s"example/index.sysml")
  val project = SysML.loadProject(new File(projectURI.toFileString))

  val resource = rs.createResource(projectURI.appendFileExtension("uml"))

  project.includes.foreach { include =>
    println(s"loading $include")
    val uri = projectURI.trimSegments(1).appendSegment(include)
    val positions = SysML.load(new File(uri.toFileString),resource)
  }

  project.mappings.foreach { mapping =>
    val Seq(modelName, rest @_*) = mapping.supplier.parts
    val model = resource.getContents.asScala.collectFirst {
      case m: Model if m.getName == modelName => m
    }
    if (rest.length == 0) { // model <|- model mapping
      val name = mapping.client.content.trim
      val clientModel = resource.getContents.asScala.collectFirst {
        case m: Model if m.getName == name => m
      }
      for {
        supplier <- model
        client <- clientModel
      } {
        val realization = UMLFactory.eINSTANCE.createRealization()
        realization.getSuppliers.add(supplier)
        realization.getClients.add(client)
        resource.getContents.add(realization)
      }
    }
  }

  resource.save(new util.HashMap)
}
