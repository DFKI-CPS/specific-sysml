package specific.sysml.parser

import scala.collection.SortedSet
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by martin on 18.04.16.
  */
trait SysMLTokens { self: Tokens =>
  sealed trait Indentation extends Token { val n: Int; val chars = "" }
  object Indentation {
    case object None extends Indentation { val n = 0 }
    case class Tabs(n: Int) extends Indentation {
      override def toString = s"$n tabs"
    }
    case class Spaces(n: Int) extends Indentation {
      override def toString = s"$n spaces"
    }
  }

  sealed trait IndentationToken extends Token { val chars = "" }
  case object SEPARATOR extends IndentationToken {
    override def toString = "end of line"
  }
  case object INDENT extends IndentationToken {
    override def toString = "start of indented block"
  }
  case object DEDENT extends IndentationToken {
    override def toString = "end of indented block"
  }
  case class UNMATCHED_DEDENT(indentation: Indentation) extends IndentationToken {
    override def toString = s"unmatched dedentation (no previous line was indented with $indentation)"
  }
  case class INCONSISTENT_INDENTATION(before: Indentation, now: Indentation) extends IndentationToken {
    override def toString = s"inconsistent indentation (started with $before and continued with $now)"
  }
  case class MIXED_INDENTATION(start: Indentation) extends IndentationToken {
    override def toString = start match {
      case Indentation.Spaces(n) => s"mixed indentation characters in one line (tab afer $n spaces)"
      case Indentation.Tabs(n) => s"mixed indentation characters in one line (space afer $n tabs)"
    }
  }

  case class Name(chars: String) extends Token {
    override def toString = s""""$chars""""
  }

  case class Number(value: Int) extends Token { def chars = value.toString }

  val keywords = SortedSet[Keyword](
    BLOCK,VALUES,PROPERTIES,OPERATIONS,REFERENCES,PORTS,PARTS,OWNED_BEHAVIORS,
    CONSTRAINTS,RECEIVE,SEND,AFTER,SECONDS,ELSE,LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,COLON,RIGHT_ARROW,LEFT_ARROW,PLUS,MINUS,HASH,TILDE,
    SLASH,COMMA,STAR,ELIPSIS,DOT,LEFT_PARENS,RIGHT_PARENS,CHOOSE,STATE_MACHINE,
    STATE
  )(Ordering.by[Keyword,(Int,String)](k => (k.chars.length, k.chars)).reverse)

  case class Constraint(chars: String) extends Token

  sealed abstract class Keyword(val chars: String) extends Token {
    override def toString = chars
  }
  case object CHOOSE extends Keyword("choose")
  case object STATE extends Keyword("state")
  case object STATE_MACHINE extends Keyword("state machine")
  case object BLOCK extends Keyword("block")
  case object VALUES extends Keyword("values")
  case object PROPERTIES extends Keyword("properties")
  case object OPERATIONS extends Keyword("operations")
  case object REFERENCES extends Keyword("references")
  case object PORTS extends Keyword("ports")
  case object PARTS extends Keyword("parts")
  case object OWNED_BEHAVIORS extends Keyword("owned behaviors")
  case object CONSTRAINTS extends Keyword("constraints")
  case object RECEIVE extends Keyword("receive")
  case object SEND extends Keyword("send")
  case object AFTER extends Keyword("after")
  case object SECONDS extends Keyword("seconds")
  case object ELSE extends Keyword("else")
  case object IN extends Keyword("in")
  case object OUT extends Keyword("out")
  case object INOUT extends Keyword("inout")
  case object LEFT_PARENS extends Keyword("(")
  case object RIGHT_PARENS extends Keyword(")")
  case object LEFT_SQUARE_BRACKET extends Keyword("[")
  case object RIGHT_SQUARE_BRACKET extends Keyword("]")
  case object COLON extends Keyword(":")
  case object RIGHT_ARROW extends Keyword("->")
  case object LEFT_ARROW extends Keyword("<-")
  case object PLUS extends Keyword("+")
  case object MINUS extends Keyword("-")
  case object HASH extends Keyword("#")
  case object TILDE extends Keyword("~")
  case object SLASH extends Keyword("/")
  case object COMMA extends Keyword(",")
  case object STAR extends Keyword("*")
  case object ELIPSIS extends Keyword("..")
  case object DOT extends Keyword(".")
}
