package specific.sysml.parser

import specific.sysml.{ModelBuilder}

import scala.io.Source
import scala.util.parsing.input.{CharSequenceReader, Reader}

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  val source = Source.fromFile("example.sysml")

  var tokens: Reader[SysMLLexer.Token] = new IndentScanner(new SysMLLexer.Scanner(source.mkString))

  SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
    case SysMLParsers.Success(b,_) =>
      val builder = new ModelBuilder("example")
      builder.addDiagrams(Seq(b))
      builder.save()
    case SysMLParsers.NoSuccess(msg,i) =>
      println(s"$msg [${i.pos}]:\n${i.pos.longString}")
  }

}
