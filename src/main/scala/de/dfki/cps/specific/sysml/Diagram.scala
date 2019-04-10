package de.dfki.cps.specific.sysml

import java.io.File

import org.eclipse.emf.ecore.resource.Resource
import specific.sysml.parser.{IndentScanner, SysMLLexer}

import scala.io.Source

object Diagram extends App {
  def load(source: File, target: Resource, includeOCL: Boolean = false, includeProfileApplcations: Boolean = true): Unit = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    println(tokens)
  }
}