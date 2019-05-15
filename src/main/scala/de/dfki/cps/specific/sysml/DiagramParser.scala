package de.dfki.cps.specific.sysml

import java.io.File

import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._


object DiagramParser extends App {
  def convert(element: NamedElement): JsValue = element match {
    case Diagram(DiagramKind.BlockDefinitionDiagram,m,n,name,members) =>
      Map(
        "title" -> Array("Block Definition Diagram", s"[${m}]", n.mkString("::"), name).toJson,
        "blocks" -> members.collect {
          case Block(name,compartments,comments) =>
            name -> Map(
              "stereotype" -> "block".toJson,
              "compartments" -> compartments.collect {
                case comp if comp.compartmentName != "references" => comp.compartmentName -> comp.content.map(convert).toJson
              }.toMap.toJson
            ).toJson
        }.toMap.toJson,
        "connectors" -> members.collect { case b: Block => b }.flatMap { from =>
          val refs = from.compartments.collect { case r: ReferencesCompartment => r }.flatMap(r => r.references)
          refs.map(to => {
            val c = to.typeAnnotation.name.parts.mkString("::")
            val opposite = to.oppositeName.flatMap(n => members.collect { case b: Block if b.rawName == c => b }.flatMap(_.members.collect{ case r: Reference if r.name == n => r  }).headOption)
            Map(
              "from" -> from.rawName.toJson,
              "to" -> c.toJson,
              "labels" -> opposite.fold(Map(
                "end" -> (to.name + to.typeAnnotation.multiplicity.fold("")(_.toString))
              ).toJson) { r => Map(
                "start" -> (r.name + r.typeAnnotation.multiplicity.fold("")(_.toString)),
                "end" -> (to.name + to.typeAnnotation.multiplicity.fold("")(_.toString))
              ).toJson }
            ).toJson
          })
         }.toJson
      ).toJson
    case default => default.toString.toJson
  }

  def load(source: File): Unit = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
      case SysMLParsers.Success(b: Diagram,_) =>
        println("diagram = " + convert(b).prettyPrint)
      case err@ SysMLParsers.NoSuccess(msg,i) =>
        println(s"parse error [${source.getName}:${i.pos.line}:${i.pos.column}]: $msg")
        println(i.pos.longString)
    }
  }

  load(new File("example.sysml"))
}