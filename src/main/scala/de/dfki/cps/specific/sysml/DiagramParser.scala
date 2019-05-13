package de.dfki.cps.specific.sysml

import java.io.File

import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._


object DiagramParser extends App {
  def convert(element: NamedElement): JsValue = element match {
    case Diagram(kind,m,n,name,members) =>
      Map(
        "name" -> name.toJson,
        "kind" -> kind.toString.toJson,
        "members" -> members.map(convert).toJson
      ).toJson
    case Block(name,compartments,comments) =>
      Map(
        "name" -> name.toJson,
        "compartments" -> compartments.map(c => convert(c)).toJson
      ).toJson
    case default => default.name.toJson
  }

  def convert(compartment: BlockCompartment): JsValue = compartment match {
    case OperationsCompartment(ops) =>
      Map(
        "name" -> "operations".toJson,
        "items" -> ops.map(convert).toJson
      ).toJson
    case default => default.compartmentName.toJson
  }

  def load(source: File): Unit = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
      case SysMLParsers.Success(b: Diagram,_) =>
        println(convert(b).prettyPrint)
      case SysMLParsers.NoSuccess(msg,i) =>
        println(s"parse error: $msg")

    }
  }

  load(new File("example/02-fsl0.sysml"))
}