package specific.sysml.parser

import scala.collection.SortedSet
import scala.util.parsing.combinator.token.Tokens

/**
  * Created by martin on 18.04.16.
  */
trait SysMLTokens extends OCLTokens { self: Tokens =>
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
    LEFT_PARENS,RIGHT_PARENS, LEFT_SQUARE_BRACKET, RIGHT_SQUARE_BRACKET,
    LEFT_BRACE,RIGHT_BRACE,
    COLON,RIGHT_ARROW,LEFT_ARROW,PLUS,MINUS,HASH,TILDE,
    SLASH,COMMA,STAR,ELIPSIS,DOT
  )(Ordering.by[Keyword,(Int,String)](k => (k.chars.length, k.chars)).reverse)

  //case class Constraint(chars: String) extends Token

  case object LEFT_BRACE extends Keyword("{")
  case object RIGHT_BRACE extends Keyword("}")
  case object COLON extends Keyword(":")
  case object LEFT_ARROW extends Keyword("<-")
  case object HASH extends Keyword("#")
  case object TILDE extends Keyword("~")
  case object COMMA extends Keyword(",")
  case object ELIPSIS extends Keyword("..")
  case object DOT extends Keyword(".")
}
