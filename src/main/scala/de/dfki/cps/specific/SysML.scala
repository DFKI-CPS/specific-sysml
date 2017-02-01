package de.dfki.cps.specific

import java.io.File
import java.util

import de.dfki.cps.specific.sysml.{Diagram, Project, Synthesis}
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import scala.util.parsing.input.{Position, Reader}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object SysML {
  def load(source: File, target: Resource): Map[EObject, Position] = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
      case SysMLParsers.Success(b: Diagram,_) =>
        val synth = new Synthesis(target)
        synth.structure(b)
        synth.naming(b)
        synth.parseConstraints(b)
        synth.messages.foreach(println)
        synth.positions.toMap
      case SysMLParsers.NoSuccess(msg,i) =>
        sys.error(s"$msg [${i.pos}]:\n${i.pos.longString}")
      case other =>
        sys.error("expected project definition")
    }
  }

  def loadProject(source: File): Project = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    SysMLParsers.phrase(SysMLParsers.project)(tokens) match {
      case SysMLParsers.Success(b: Project,_) =>
        b
      case SysMLParsers.NoSuccess(msg,i) =>
        sys.error(s"$msg [${i.pos}]:\n${i.pos.longString}")
    }
  }
}
