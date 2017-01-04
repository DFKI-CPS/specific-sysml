package de.dfki.cps.specific.sysml.mappings

import java.io.File

import de.dfki.cps.specific.sysml
import de.dfki.cps.specific.sysml.mappings.Persistence.ParseException
import de.dfki.cps.specific.sysml.{Diagram, Synthesis}
import de.dfki.cps.specific.sysml.mappings.Persistence.ParseState.{Header, Ok}
import de.dfki.cps.specific.sysml.parser.{ParseError, Severity}
import org.eclipse.uml2.uml.{Model, NamedElement, Namespace}
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import scala.util.parsing.input.{NoPosition, Position}

/**
  * Created by martin on 1/4/17.
  */
object Persistence {
  case class ParseException(errors: Seq[ParseError]) extends Throwable

  sealed trait ParseState {
    val errors: Seq[ParseError]
    def parseLine(error: String => ParseError, line: String): ParseState
  }

  object ParseState {
    val initial: ParseState = Header(None,None)

    val headerFrom = raw"""^\s*from\s*:\s*([\w\.]+)\s*(?:--.*)?$$""".r
    val headerTo   = raw"""^\s*to\s*:\s*([\w\.]+)\s*(?:--.*)?$$""".r
    val headerEnd  = raw"""^\s*---.*$$""".r

    case class Header(from: Option[Model], to: Option[Model]) extends ParseState {
      val errors = Seq.empty
      def parseLine(error: String => ParseError, line: String) = line match {
        case headerFrom(from) =>
          println(s"loading model $from")
          val input = Source.fromFile(from)
          val tokens = new IndentScanner(new SysMLLexer.Scanner(input.mkString))
          SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
            case SysMLParsers.Success(b: Diagram,_) =>
              val synth = new Synthesis("from")
              synth.structure(b)
              synth.naming(b)
              synth.parseConstraints(b)
              if (synth.messages.isEmpty) this.copy(from=Some(synth.model))
              else Failed(synth.messages)
            case SysMLParsers.NoSuccess(msg,i) =>
              Failed(Seq(ParseError(from,i.pos,Severity.Error,msg)))
          }
        case headerTo(to) =>
          println(s"loading model $to")
          val input = Source.fromFile(to)
          val tokens = new IndentScanner(new SysMLLexer.Scanner(input.mkString))
          SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
            case SysMLParsers.Success(b: Diagram,_) =>
              val synth = new Synthesis("to")
              synth.structure(b)
              synth.naming(b)
              synth.parseConstraints(b)
              if (synth.messages.isEmpty) this.copy(to=Some(synth.model))
              else Failed(synth.messages)
            case SysMLParsers.NoSuccess(msg,i) =>
              Failed(Seq(ParseError(to,i.pos,Severity.Error,msg)))
          }
        case headerEnd() => (from,to) match {
          case (None,None) => Failed(Seq(error("header ends before from- or to-mapping was declared")))
          case (Some(_),None) => Failed(Seq(error("header ends before from-mapping was declared")))
          case (None,Some(_)) => Failed(Seq(error("header ends before to-mapping was declared")))
          case (Some(from),Some(to)) => Ok(Vector.empty,from,to,Set.empty,Vector.empty)
        }
        case other => Failed(Seq(error("invalid header line")))
      }
    }

    val indentation = raw"""^(\s*)(\w+)\s*(?::\s*(.*))?$$""".r

    case class Ok(indentStack: Vector[(String,NamedElement)] = Vector.empty, from: Model, to: Model, mappings: Set[Mapping[_]], errors: Seq[ParseError]) extends ParseState {
      def parseLine(error: String => ParseError, line: String) = {
        var ln = line
        var ns: NamedElement = from
        var s = indentStack
        while (s.nonEmpty && ln.startsWith(s.head._1)) {
          ns = s.head._2
          ln = ln.drop(s.head._1.length)
          s = s.tail
        }
        ln match {
          case indentation(i,f,t) =>
            if (i.nonEmpty && s.nonEmpty) Failed(Seq(error("invalid indentation")))
            ns match {
              case n: Namespace =>
                n.getMember(f) match {
                  case null => Failed(Seq(error(s"$f is not a member of ${ns.getName}")))
                  case other =>
                    val mapping = Option(t).map { expr =>
                      DataMapping(other,expr)
                    }
                    this.copy(
                     indentStack = indentStack.dropRight(s.length) :+ (i,other),
                     mappings = mappings ++ mapping
                   )
                }
              case other =>
                Failed(Seq(error(s"${ns.getName} is not a namespace")))
            }
          case other => Failed(Seq(error("invalid line")))
        }
      }
    }

    case class Failed(errors: Seq[ParseError]) extends ParseState {
      def parseLine(error: String => ParseError, line: String) = this
    }
  }

  def load(file: String): Set[Mapping[_]] = {
    val res = Source.fromFile(file).getLines().zipWithIndex.foldLeft(ParseState.initial) {
      case (state, (l, n)) => state.parseLine(s => ParseError(file,new Position {
        def column = 0
        def line = n
        protected def lineContents = l
      },Severity.Error,s), l)
    }
    res match {
      case ok: Ok if ok.errors.isEmpty => ok.mappings
      case head: Header => throw ParseException(Seq(ParseError(file,NoPosition,Severity.Error,"no valid header found")))
      case other => throw ParseException(other.errors)
    }
  }

  def save(file: String, mappings: Set[Mapping[_]]) = ???
}

object Test extends App {
  try {
    val res = Persistence.load("./example.mapping")
    res.foreach(println)
  } catch {
    case ParseException(msgs) => msgs.foreach(println)
  }
}