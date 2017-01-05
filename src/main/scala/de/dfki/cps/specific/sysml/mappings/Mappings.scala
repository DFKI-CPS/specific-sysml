package de.dfki.cps.specific.sysml.mappings

import java.io.File
import java.net.URI

import org.eclipse.uml2.uml.{Model, NamedElement, Namespace, Transition}
import specific.sysml.parser.{IndentScanner, SysMLLexer}

import scala.io.Source
import scala.util.parsing.input.Reader

case class Mappings(from: Model, to: Model, mappings: Seq[Mapping[_]])

sealed abstract trait Mapping[T <: NamedElement] {
  val source: T
}

case class DataMapping[T <: NamedElement](
  source: T,
  expr: String) extends Mapping[T]

case class BehaviorMapping(
  source: Transition,
  ins: Set[Transition],
  outs: Set[Transition]) extends Mapping[Transition]

object Mappings {
  def load(uri: URI): Mappings = {
    val absolute = new File(".").toURI.resolve(uri)
    val source = Source.fromURI(absolute)

    var tokens: Reader[SysMLLexer.Token] = new IndentScanner(new SysMLLexer.Scanner(source.mkString))

    def mappings(elem: NamedElement)(m: AST.Mapping): Seq[Mapping[_]] = m match {
      case AST.Retain(src,children) =>
        val n = elem match {
          case ns: Namespace => ns.getMember(src)
          case other => null
        }
        if (n == null) sys.error(s"$src is not a member of ${elem.getName}")
        children.flatMap(mappings(n))
      case AST.DataRefinement(src,expr,children) =>
        val n = elem match {
          case ns: Namespace => ns.getMember(src)
          case other => null
        }
        if (n == null) sys.error(s"$src is not a member of ${elem.getName}")
        val mapping = DataMapping(n,expr.content)
        mapping +: children.flatMap(mappings(n))
    }

    MappingsParsers.phrase(MappingsParsers.file)(tokens) match {
      case MappingsParsers.Success(b: AST.File,_) =>
        val absFrom = absolute.resolve(b.header.from.url)
        val absTo = absolute.resolve(b.header.to.url)
        val from = de.dfki.cps.specific.sysml.Model.load(absFrom)
        val to = de.dfki.cps.specific.sysml.Model.load(absTo)
        val ms = b.mappings.flatMap(mappings(from)(_))
        Mappings(from,to,ms)
      case MappingsParsers.NoSuccess(msg,i) =>
        println(msg)
        println(i.pos.longString)
        sys.error("msg")
    }
  }
}

object MappingsTest extends App {
  val ms = Mappings.load(URI.create("./example.mapping"))
  println(ms)
}