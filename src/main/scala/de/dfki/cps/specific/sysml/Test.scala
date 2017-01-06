package de.dfki.cps.specific.sysml

import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import scala.util.parsing.input.Reader

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
      val synth = new Synthesis("example")
      synth.structure(b)
      synth.naming(b)
      synth.parseConstraints(b)
      synth.save()
    case SysMLParsers.NoSuccess(msg,i) =>
      println(s"$msg [${i.pos}]:\n${i.pos.longString}")
  }

}
