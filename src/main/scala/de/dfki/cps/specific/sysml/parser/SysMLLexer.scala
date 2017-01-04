package specific.sysml.parser

import de.dfki.cps.specific.ocl.parser.OclLexer

import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator.lexical.{Lexical, Scanners}
import scala.util.parsing.input.CharArrayReader.EofCh

object SysMLLexer extends OclLexer {
  import SysMLTokens._

  override val delimiters = SysMLTokens.delimiters

  override def token = indentation | sysmlComment | super.token

  def lineBreak = ( '\r' ~ '\n' | '\n' | '\r' )

  def indentation = named("indentation", lineBreak ~!
  ( count1(' ') <~ noTabs ^^ Indentation.Spaces
  | count1('\t') <~ noSpaces ^^ Indentation.Tabs
  | success(Indentation.None) ) ^^ (_._2))

  override def comment = not('/' ~ '*' ~ '*') ~> super.comment

  def sysmlComment = ('/' ~ '*' ~ '*') ~!> blockComment("", 0) ^^ (_.trim) ^^ SysmlComment

  def noTabs =
    success() ~! (
      not('\t')
    | positioned(failure("indentation starting with spaces must not contain tabs")) )

  def noSpaces =
    success() ~! (
      not(' ')
    | positioned(failure("indentation starting with tabs must not contain spaces")) )

  def count1[T](parser: Parser[T]): Parser[Int] =
    rep1(parser) ^^ (_.length)

  override def whitespaceChar = elem("whitespace", ch => ch != '\r' && ch != '\n' && ch.isWhitespace)
}
