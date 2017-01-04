package specific.sysml.parser

import scala.util.parsing.input.{Position, Reader}

/**
  * Created by martin on 19.04.16.
  */
case class IndentScanner(input: Reader[SysMLLexer.Token], indentStack: List[SysMLTokens.Indentation] = Nil) extends Reader[SysMLLexer.Token] {
  override def offset: Int = input.offset

  def atEnd: Boolean = input.atEnd && indentStack.isEmpty

  lazy val (firstf: (() => SysMLLexer.Token), restf: (() => Reader[SysMLLexer.Token]), posf: (() => Position)) = if (input.atEnd) indentStack match {
    case i :: is => (() => SysMLTokens.DEDENT,() => IndentScanner(input,is), () => input.pos)
    case Nil => (() => SysMLLexer.EOF.asInstanceOf[SysMLLexer.Token],() => this, () => input.pos)
  } else input.first match {
    case SysMLTokens.Indentation.None => indentStack match {
      case Nil => (() => SysMLTokens.SEPARATOR, () => IndentScanner(input.rest,indentStack), () => input.pos)
      case i :: is => (() => SysMLTokens.DEDENT, () => IndentScanner(input,is), () => input.pos)
    }
    case s@SysMLTokens.Indentation.Spaces(n) => indentStack match {
      case Nil => (() => SysMLTokens.INDENT,() => IndentScanner(input.rest, s::Nil), () => input.pos)
      case SysMLTokens.Indentation.Spaces(n2) :: is =>
        if (n == n2) (() => SysMLTokens.SEPARATOR, () => IndentScanner(input.rest, indentStack), () => input.pos)
        else if (n > n2) (() => SysMLTokens.INDENT, () => IndentScanner(input.rest, s::indentStack), () => input.rest.pos)
        else if (indentStack.contains(s)) (() => SysMLTokens.DEDENT, () => IndentScanner(input,is), () => input.rest.pos)
        else (() => SysMLTokens.UNMATCHED_DEDENT(s), () => IndentScanner(input.rest,indentStack), () => input.rest.pos)
      case (b@SysMLTokens.Indentation.Tabs(n2)) :: is =>
        (() => SysMLTokens.INCONSISTENT_INDENTATION(b, s), () => IndentScanner(input.rest,indentStack), () => input.rest.pos)
      case other => sys.error("invalid indent stack state")
    }
    case s@SysMLTokens.Indentation.Tabs(n) => indentStack match {
      case Nil => (() => SysMLTokens.INDENT,() => IndentScanner(input.rest, s::Nil),() => input.pos)
      case SysMLTokens.Indentation.Tabs(n2) :: is =>
        if (n == n2) (() => SysMLTokens.SEPARATOR,() => IndentScanner(input.rest, indentStack),() => input.pos)
        else if (n > n2) (() => SysMLTokens.INDENT,() => IndentScanner(input.rest, s::indentStack),() => input.pos)
        else if (indentStack.contains(s)) (() => SysMLTokens.DEDENT,() => IndentScanner(input,is),() => input.pos)
        else (() => SysMLTokens.UNMATCHED_DEDENT(s),() => IndentScanner(input.rest,indentStack),() => input.rest.pos)
      case (b@SysMLTokens.Indentation.Spaces(n2)) :: is =>
        (() => SysMLTokens.INCONSISTENT_INDENTATION(b, s),() => IndentScanner(input.rest,indentStack),() => input.rest.pos)
      case other => sys.error("invalid indent stack state")
    }
    case other: SysMLLexer.Token => (() => other,() => IndentScanner(input.rest,indentStack),() => input.pos)
  }

  def first: SysMLLexer.Token = firstf()
  def rest: Reader[SysMLLexer.Token] = restf()
  def pos: Position = posf()

  override def source: CharSequence = input.source
}

class IndentationIgnorer(input: Reader[SysMLLexer.Token], level: Int = 0) extends Reader[SysMLLexer.Token] {
  private lazy val (atEndf: (() => Boolean), firstf: (() => SysMLLexer.Token), restf: (() => Reader[SysMLLexer.Token]), posf: (() => Position)) =
    if (input.atEnd) (() => true, () => input.first, () => input.rest, () => input.pos)
    else {
      input.first match {
        case SysMLTokens.INDENT =>
          val r = new IndentationIgnorer(input.rest, level + 1)
          (() => r.atEnd, () => r.first, () => r.rest, () => r.pos)
        case SysMLTokens.DEDENT if level == 0 =>
          (() => input.atEnd, () => input.first, () => input.rest, () => input.pos)
        case SysMLTokens.DEDENT =>
          val r = new IndentationIgnorer(input.rest, level - 1)
          (() => r.atEnd, () => r.first, () => r.rest, () => r.pos)
        case other =>
          (() => false, () => input.first, () => new IndentationIgnorer(input.rest, level), () => input.pos)
      }
    }

  def atEnd: Boolean = atEndf()
  def first: SysMLLexer.Token = firstf()
  def rest: Reader[SysMLLexer.Token] = restf()
  def pos: Position = posf()
  override def source: CharSequence = input.source
  override def offset: Int = input.offset
}