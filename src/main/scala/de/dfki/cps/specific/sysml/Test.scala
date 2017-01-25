package de.dfki.cps.specific.sysml

import java.util

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

  val source = Source.fromFile("example.sysml")
  var tokens: Reader[SysMLLexer.Token] = new IndentScanner(new SysMLLexer.Scanner(source.mkString))

  SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
    case SysMLParsers.Success(b: Diagram,_) =>
      val res = rs.createResource(URI.createFileURI("example.ecore"))
      val synth = new Synthesis("example")
      synth.structure(b)
      synth.naming(b)
      synth.parseConstraints(b)
      res.getContents.addAll(synth.temp.getContents)
      res.save(new util.HashMap)
    case SysMLParsers.NoSuccess(msg,i) =>
      println(s"$msg [${i.pos}]:\n${i.pos.longString}")
  }

}
