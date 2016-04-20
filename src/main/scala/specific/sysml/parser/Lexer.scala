package specific.sysml.parser

import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator.lexical.{Lexical, Scanners}
import scala.util.parsing.input.CharArrayReader.EofCh

object Lexer extends Lexical with SysMLTokens {
  def token = indentation | keyword | simpleName | number //| constraint

  def lineBreak = ( '\r' ~ '\n' | '\n' | '\r' )

  def indentation = lineBreak ~!
  ( rep1(' ') ~! (not('\t') | positioned(failure("indentation starting with spaces must not contain tabs"))) ^^ { case ss ~ _ => Indentation.Spaces(ss.length) }
  | rep1('\t') ~! (not(' ') | positioned(failure("indentation starting with tabs must not contain spaces"))) ^^ { case ts ~ _ => Indentation.Tabs(ts.length) }
  | success(Indentation.None) ) ^^ (_._2)

  /** @see OCL 12-01-01 Section 9.4.4 simpleNameCS */
  def simpleName: Parser[Name] =
    ( '_' ~> '\'' ~> (stringChar.* <~ '\'') ^^ (cs => Name(cs.mkString))
    | nameStartChar ~! nameChar.* ^^ { case hd ~ tl => Name(hd.toString + tl.mkString) })

  def stringChar = char | escapeSequence

  def hex = digit | elem("hex char", ch => ('a' to 'f' contains ch) || ('A' to 'F' contains ch))

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
    | '\\' ~> 'x' ~> hex ~ hex ^^ { case h1 ~ h2 => ' ' }
    | '\\' ~> 'u' ~> hex ~ hex ~ hex ~ hex ^^ { case h1 ~ h2 ~ h3 ~ h4 => ' ' } )

  def nameChar: Parser[Char] = nameStartChar | digit

  def nameStartChar: Parser[Char] = elem("name start char", ch => ch.isLetter || ch == '_' || ch == '$')

  def whitespace: Parser[Any] = rep( whitespaceChar | comment )

  def number: Parser[Number] = rep1(digit) ^^ { cs => Number(cs.mkString.toInt) }

  def keyword = keywords.map { case k => acceptSeq(k.chars) ~ guard(whitespace) ^^^ k }
      .foldRight[Parser[Token]] { failure("no delimiters") } { (x,y) => x ||| y }

  override def whitespaceChar = elem("whitespace", ch => ch != '\r' && ch != '\n' && ch.isWhitespace)

  //def constraint: Parser[Constraint] = '{' ~> (allExcept(EofCh,'}') *) <~ '}' ^^ (cs => Constraint(cs.mkString))

  def comment =
    ( '-' ~ '-' ~! (allExcept(EofCh, '\n')* )
    | '/' ~ '*' ~! blockComment(1) )

  def blockComment(level: Int): Parser[Any] =
    ( '/' ~ '*' ~! blockComment(level + 1)
    | '/' ~! blockComment(level)
    | '*' ~ '/' ~! (if (level > 1) blockComment(level - 1) else success())
    | '*' ~! blockComment(level)
    | (allExcept('*', '/') *) )

  def allExcept(cs: Char*) = elem(s"none of (${cs.mkString(", ")})", !cs.contains(_))
}
