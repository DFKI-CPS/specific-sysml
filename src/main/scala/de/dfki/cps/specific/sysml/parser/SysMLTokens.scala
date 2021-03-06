package specific.sysml.parser

import de.dfki.cps.specific.ocl.parser.OclTokens

import scala.collection.SortedSet
import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.Positional

/**
  * Created by martin on 18.04.16.
  */
object SysMLTokens {
  type Token = OclTokens.Token
  type Delimiter = OclTokens.Delimiter

  case class SysmlComment(chars: String) extends Token with Positional

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
      case Indentation.None => sys.error("invalid indentation state")
    }
  }

  val delimiters = OclTokens.delimiters ++ SortedSet[Delimiter](
    ELIPSIS, HASH, LEFT_ARROW, TILDE
  ) (Ordering.by[Delimiter,(Int,String)](k => (k.chars.length, k.chars)).reverse)

  case object ELIPSIS extends Delimiter("..")
  case object HASH extends Delimiter("#")
  case object LEFT_ARROW extends Delimiter("<-")
  case object TILDE extends Delimiter("~")
}
