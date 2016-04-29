package specific.sysml.parser

import specific.sysml.ModelBuilder
import specific.sysml.synthesis.Synthesizer

import scala.io.Source
import scala.util.parsing.input.{CharSequenceReader, Reader}

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  val source = Source.fromFile("example.sysml")

  var tokens: Reader[SysMLLexer.Token] = new IndentScanner(new SysMLLexer.Scanner(source.mkString))

  println("parsing")
  SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
    case SysMLParsers.Success(b,_) =>
      println("passed")
      println("synthesizing")
      val builder = new Synthesizer("example")
      val res = builder.synthesize(b).complete()
      if (res.isSuccess) println("success")
      else {
        println("synthesis failed")
        println(res)
        res.messages.foreach(println)
      }
      builder.save()
    case SysMLParsers.NoSuccess(msg,i) =>
      println(s"$msg [${i.pos}]:\n${i.pos.longString}")
  }

}
