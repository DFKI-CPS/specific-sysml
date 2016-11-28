package specific.sysml

import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import scala.util.parsing.input.Reader

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  val source = Source.fromFile("example.sysml")

  var tokens: Reader[SysMLLexer.Token] = new IndentScanner(new SysMLLexer.Scanner(source.mkString))

  SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
    case SysMLParsers.Success(b: Diagram,_) =>
      val synth = new Synthesis("example")
      synth.structure(b)
      synth.naming(b)
      synth.save()
    case SysMLParsers.NoSuccess(msg,i) =>
      println(s"$msg [${i.pos}]:\n${i.pos.longString}")
  }

}
