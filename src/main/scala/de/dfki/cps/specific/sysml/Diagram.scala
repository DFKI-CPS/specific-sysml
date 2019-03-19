package de.dfki.cps.specific.sysml

import java.io.File

import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import scala.util.parsing.input.Position

object Diagram extends App {
  def load(source: File, target: Resource, includeOCL: Boolean = false, includeProfileApplcations: Boolean = true): Map[EObject, Position] = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
      case SysMLParsers.Success(b: Diagram,_) =>
        val synth = new Synthesis(source.toString,target)
        synth.includeOCL = includeOCL
        synth.includeProfileApplications = includeProfileApplcations
        synth.structure(b)
        synth.naming(b)
        synth.parseConstraints(b)
        synth.positions.toMap
      case SysMLParsers.NoSuccess(msg,i) =>
        target.getErrors.add(SemanticMessage(source.toString,i.pos,msg,None))
        Map.empty
    }
  }

}
