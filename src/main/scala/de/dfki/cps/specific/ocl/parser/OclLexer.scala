package de.dfki.cps.specific.ocl.parser

import de.dfki.cps.specific.util.ParserHelpers

import scala.collection.SortedSet
import scala.collection.immutable.NumericRange
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.CharArrayReader._
import scala.math.BigInt

object OclLexer extends OclLexer

/**
  * Created by martin on 25.04.16.
  */
trait OclLexer extends Lexical with ParserHelpers {
  type Token = OclTokens.Token
  import OclTokens._

  val delimiters = OclTokens.delimiters

  def token: Parser[Token] = keyword | delimiter | simpleName | primitiveLiteral
  def whitespace: Parser[Any] = (comment | whitespaceChar).*

  //// KEYWORDS AND DELIMITERS

  def keyword = named("keyword", oclKeywords.map {
    case k => acceptSeq(k.chars) ~ not(nameChar) ^^^ k
  }.reduceRight[Parser[Token]] { case (x,y) => x ||| y })

  def delimiter = named("delimiter", delimiters.map {
    case k => acceptSeq(k.chars) ^^^ k
  }.reduceRight[Parser[Token]] { case (x,y) => x ||| y })

  //// NAMES

  def simpleName =
    ( nameStartChar ~ nameChar.* ^^ mkList ^^ (_.mkString) ^^ SimpleName
    | '_' ~> rep1sep(stringPart, whitespace.*) ^^ concat ^^ SimpleName )
  def nameStartChar =
    letter | '_' | '$'
  def nameChar =
    nameStartChar | digit

  //// LITERALS

  def primitiveLiteral = stringLiteral | integerLiteral

  def integerLiteral =
    integerPart ^^ IntegerLiteral

  def integerPart =
    ( '0' ^^^ BigInt(0) | ('1' to '9') ~ digit.* ^^ mkList ^^ bigInt )

  override def digit = '0' to '9'

  def fractionalPart = '.' ~> digit.+ ^^ bigInt

  case class Exponent()

  def exponentPart = (elem('e') | 'E') ~> opt(elem('+') | '-') ~ integerPart ^^ {
    case e ~ i => if(e.contains('-')) -i else i
  }

  def realLiteral =
    ( exponentPart ~ integerPart ~ opt(fractionalPart) ^^ {
      case e ~ i ~ Some(f) => BigDecimal(s"$i.${f}e$e")
      case e ~ i ~ None => BigDecimal(s"${i}e$e")
    }
    | integerPart ~ fractionalPart ^^ {
      case i ~ f => BigDecimal(s"$i.$f")
    })

  def invalidLiteral = ???

  def stringLiteral =
    rep1sep(stringPart, whitespace.*) ^^ concat ^^ StringLiteral

  def stringPart =
    '\'' ~> (stringChar.* <~ '\'') ^^ (_.mkString)

  def stringChar = named("string character", char | escapeSequence)

  def hex = ('0' to '9') | ('A' to 'F') | ('a' to 'f')

  val escapeChars = Set('\b', '\t', '\n', '\f', '\r', '\"', '\'', '\\')

  def char = elem("string char", ch => !ch.isControl && !escapeChars.contains(ch))

  def escapeSequence: Parser[Char] = '\\' ~!
    ( 'b' ^^^ '\b'
    | 't' ^^^ '\t'
    | 'n' ^^^ '\n'
    | 'f' ^^^ '\f'
    | 'r' ^^^ '\r'
    | '"' ^^^ '"'
    | '\'' ^^^ '\''
    | '\\' ^^^ '\\'
    | 'x' ~> hex ~ hex ^^ {
      case h1 ~ h2 => Integer.parseInt(s"$h1$h2", 16).toChar
    }
    | 'u' ~> hex ~ hex ~ hex ~ hex ^^ {
      case h1 ~ h2 ~ h3 ~ h4 =>
        Integer.parseInt(s"$h1$h2$h3$h4", 16).toChar
    }
    | err("invalid escape sequence") ) ^^ (_._2)

  //// COMMENTS

  def comment = named("comment",
    ( '-' ~ '-' ~! (allExcept(EofCh, '\n')* )
    | '/' ~ '*' ~! blockComment("",1) ))

  def blockComment(text: String, level: Int): Parser[String] = named("blockComment",
    ( ('/' ~ '*') ~!> blockComment(text, level + 1)
    | '/' ~! blockComment(text + "/", level) ^^ (_._2)
    | ('*' ~ '/') ~!> (if (level > 1) blockComment(text, level - 1) else success(text))
    | '*' ~! blockComment(text, level) ^^ (_._2)
    | allExcept('*', '/').* >> (text2 => blockComment(text + text2.mkString, level)) )
  )

  //// HELPERS

  implicit def rangeToParser(range: NumericRange[Char]): Parser[Elem] =
    elem(s"range", range.contains)

  def bigInt = (x: Seq[Char]) => BigInt(x.mkString)

  def concat = (x: Seq[String]) => x.reduce(_ + _)

  def mkString = (x: Seq[Char]) => x.mkString
}