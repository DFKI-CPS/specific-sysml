package de.dfki.cps.specific.util

import scala.util.parsing.combinator.Parsers

/**
  * Created by martin on 25.04.16.
  */
trait ParserHelpers { self: Parsers =>
  protected def enclosed[T](left: Parser[_], middle: Parser[T], right: Parser[_]) =
    left ~> (middle <~ right)

  protected def named[T](name: String, p: Parser[T]): Parser[T] =
    p | Parser(i => Failure(s"expected $name but found ${i.first.toString}", i))

  implicit class ParserExts[T](val p: Parser[T]) {
    @inline
    def ~!>[U](u: => Parser[U]): Parser[U] = p ~! u ^^ (_._2)
  }

  def allExcept(cs: self.Elem*) =
    elem(s"none of (${cs.mkString(", ")})", !cs.contains(_))
}

