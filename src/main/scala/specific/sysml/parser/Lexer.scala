package specific.sysml.parser

import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator.lexical.{Lexical, Scanners}
import scala.util.parsing.input.CharArrayReader.EofCh

object Lexer extends Lexical with SysMLTokens {
  def token = indentation | keyword | simpleName | delimiter | number //| constraint

  def lineBreak = ( '\r' ~ '\n' | '\n' | '\r' )

  def indentation = named("indentation", lineBreak ~!
  ( count1(' ') <~ noTabs ^^ Indentation.Spaces
  | count1('\t') <~ noSpaces ^^ Indentation.Tabs
  | success(Indentation.None) ) ^^ (_._2))

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

  /** @see OCL 12-01-01 Section 9.4.4 simpleNameCS */
  def simpleName: Parser[Name] = named("simple name",
    ( '_' ~> '\'' ~> (stringChar.* <~ '\'') ^^ (_.mkString)
    | nameStartChar ~! nameChar.* ^^ mkList ^^ (_.mkString)) <~ not(nameChar) ^^ Name )

  def stringChar = named("string character", char | escapeSequence)

  def hex = named("hex digit", digit | elem("hex char", ch => ('a' to 'f' contains ch) || ('A' to 'F' contains ch)) ^^ (_.toLower))

  val escapeChars = Set('\b', '\t', '\n', '\f', '\r', '\"', '\'', '\\')

  def char = elem("string char", ch => !ch.isControl && !escapeChars.contains(ch))

  def escapeSequence: Parser[Char] =
    ( '\\' ~ 'b' ^^^ '\b'
    | '\\' ~ 't' ^^^ '\t'
    | '\\' ~ 'n' ^^^ '\n'
    | '\\' ~ 'f' ^^^ '\f'
    | '\\' ~ 'r' ^^^ '\r'
    | '\\' ~ '"' ^^^ '"'
    | '\\' ~ '\'' ^^^ '\''
    | '\\' ~> 'x' ~> hex ~ hex ^^ { case h1 ~ h2 => ' ' } // TODO
    | '\\' ~> 'u' ~> hex ~ hex ~ hex ~ hex ^^ { case h1 ~ h2 ~ h3 ~ h4 => ' ' } ) // TODO

  def nameChar: Parser[Char] = nameStartChar | digit

  def nameStartChar: Parser[Char] = elem("name start char", ch => ch.isLetter || ch == '_' || ch == '$')

  def whitespace: Parser[Any] = named("whitespace", rep( whitespaceChar | comment ))

  def number: Parser[Number] = named("number", rep1(digit) ^^ (_.mkString.toInt) ^^ Number)

  def keyword = named("keyword", oclKeywords.map { case k => acceptSeq(k.chars) ~ not(nameChar) ^^^ k }
      .foldRight[Parser[Token]] { failure("no delimiters") } { (x,y) => x ||| y })

  def delimiter = named("delimiter", (oclDelimiters ++ delimiters).map { case k => acceptSeq(k.chars) ^^^ k }
    .foldRight[Parser[Token]] { failure("no delimiters") } { (x,y) => x ||| y })

  override def whitespaceChar = elem("whitespace", ch => ch != '\r' && ch != '\n' && ch.isWhitespace)

  def named[T](name: String, p: Parser[T]): Parser[T] = p | Parser(i => Failure(s"expected $name but found ${i.first.toString}", i))

  def comment = named("comment",
    ( '-' ~ '-' ~! (allExcept(EofCh, '\n')* )
    | '/' ~ '*' ~! blockComment(1) ))

  def blockComment(level: Int): Parser[Any] = named("blockComment",
    ( '/' ~ '*' ~! blockComment(level + 1)
    | '/' ~! blockComment(level)
    | '*' ~ '/' ~! (if (level > 1) blockComment(level - 1) else success())
    | '*' ~! blockComment(level)
    | (allExcept('*', '/') *) ) )

  def allExcept(cs: Char*) = elem(s"none of (${cs.mkString(", ")})", !cs.contains(_))
}
