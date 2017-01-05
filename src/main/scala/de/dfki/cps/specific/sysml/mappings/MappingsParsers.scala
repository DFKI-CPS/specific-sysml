package de.dfki.cps.specific.sysml.mappings

import java.net.URI

import de.dfki.cps.specific.ocl.parser.OclTokens._
import de.dfki.cps.specific.ocl.parser.{OclLexer, OclParsers}
import de.dfki.cps.specific.sysml.{ConstraintType, UnprocessedConstraint}
import specific.sysml.parser.{IndentScanner, SysMLLexer}
import specific.sysml.parser.SysMLTokens.{DEDENT, INDENT, SEPARATOR}

import scala.io.Source
import scala.util.control.NonFatal
import scala.util.parsing.input.{Positional, Reader}

object AST {
  case class File(header: Header, mappings: Seq[Mapping])
  case class FromDecl(url: URI) extends Positional
  case class ToDecl(url: URI) extends Positional
  case class Header(from: FromDecl, to: ToDecl) extends Positional
  sealed trait Mapping extends Positional
  case class Retain(source: String, children: Seq[Mapping]) extends Mapping
  case class DataRefinement(source: String, expr: UnprocessedConstraint, children: Seq[Mapping]) extends Mapping
}

object MappingsParsers extends OclParsers {
  override type Elem = OclLexer.Token

  def file: Parser[AST.File] = named("mappings file",
    SEPARATOR.* ~> ((header ~! SEPARATOR.+ ~! mappings) ^^ { case h~_~ms => AST.File(h,ms) }) <~ SEPARATOR.*
  )

  def header: Parser[AST.Header] = positioned(named("header",
   (from ~ (SEPARATOR ~> to) ^^ { case from~to => AST.Header(from,to) })
  | failure("malformed header")
  ))

  private def captureURI: Parser[URI] = Parser[URI] { input =>
    val first = input.offset
    (allExcept(SEPARATOR,INDENT,DEDENT).+)(input) match {
      case Success(_,next) =>
        val text = input.source.subSequence(first + 1, next.offset).toString.trim
        try {
          Success(URI.create(text),next)
        } catch {
          case NonFatal(e) => Failure(s"invalid url '$text'", next)
        }
      case err: NoSuccess => err
    }
  }

  private def captureConstraint = Parser[UnprocessedConstraint] { input =>
    val start = input.pos
    val first = input.offset
    allExcept(SEPARATOR,INDENT,DEDENT)(input) match {
      case Success(_,next) =>
        val c = UnprocessedConstraint(ConstraintType.Query, None, input.source.subSequence(first + 1, next.offset).toString)
        c.pos = start
        Success(c,next)
      case err: NoSuccess => err
    }
  }

  def from: Parser[AST.FromDecl] = positioned(named("declaration of refinement source",
    "from" ~> (COLON ~> captureURI) ^^ AST.FromDecl
  ))

  def to: Parser[AST.ToDecl] = positioned(named("declaration of refinement target",
    "to" ~> (COLON ~> captureURI) ^^ AST.ToDecl
  ))

  def mappings: Parser[Seq[AST.Mapping]] = named("mappings",
    repsep(mapping,SEPARATOR.+)
  )

  def mapping: Parser[AST.Mapping] = positioned(named("mapping",
    dataMapping | retain
  ))

  def retain: Parser[AST.Retain] = named("retain mapping",
    simpleName ~ (INDENT ~> (
      mappings
    ) <~ DEDENT).? ^^ { case n ~ ms => AST.Retain(n.name, ms.getOrElse(Seq.empty))}
  )

  def dataMapping: Parser[AST.DataRefinement] = named("data mapping",
    simpleName ~ (RIGHT_ARROW ~! captureConstraint) ~ (INDENT ~> (
      mappings
    ) <~ DEDENT).? ^^ { case n ~ (_ ~ expr) ~ ms => AST.DataRefinement(n.name,expr,ms.getOrElse(Seq.empty)) }
  )
}