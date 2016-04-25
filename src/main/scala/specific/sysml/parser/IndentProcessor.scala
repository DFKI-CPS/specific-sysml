package specific.sysml.parser


import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

/**
  * Created by martin on 19.04.16.
  */
class IndentScanner(private var input: Reader[SysMLLexer.Token], indentStack: List[SysMLLexer.Indentation] = Nil) extends Reader[SysMLLexer.Token] {
  def atEnd = input.atEnd && indentStack.isEmpty

  lazy val (first: SysMLLexer.Token, rest: Reader[SysMLLexer.Token], pos: Position) = if (input.atEnd) indentStack match {
    case i :: is => (SysMLLexer.DEDENT,new IndentScanner(input,is),input.pos)
    case Nil => (SysMLLexer.EOF,this,input.pos)
  } else input.first match {
    case SysMLLexer.Indentation.None => indentStack match {
      case Nil => (SysMLLexer.SEPARATOR,new IndentScanner(input.rest,indentStack),input.pos)
      case i :: is => (SysMLLexer.DEDENT,new IndentScanner(input,is),input.pos)
    }
    case s@SysMLLexer.Indentation.Spaces(n) => indentStack match {
      case Nil => (SysMLLexer.INDENT,new IndentScanner(input.rest, s::Nil),input.pos)
      case SysMLLexer.Indentation.Spaces(n2) :: is =>
        if (n == n2) (SysMLLexer.SEPARATOR,new IndentScanner(input.rest, indentStack),input.pos)
        else if (n > n2) (SysMLLexer.INDENT,new IndentScanner(input.rest, s::indentStack),input.rest.pos)
        else if (indentStack.contains(s)) (SysMLLexer.DEDENT,new IndentScanner(input,is),input.rest.pos)
        else (SysMLLexer.UNMATCHED_DEDENT(s),new IndentScanner(input.rest,indentStack),input.rest.pos)
      case (b@SysMLLexer.Indentation.Tabs(n2)) :: is =>
        (SysMLLexer.INCONSISTENT_INDENTATION(b, s),new IndentScanner(input.rest,indentStack),input.rest.pos)
    }
    case s@SysMLLexer.Indentation.Tabs(n) => indentStack match {
      case Nil => (SysMLLexer.INDENT,new IndentScanner(input.rest, s::Nil),input.pos)
      case SysMLLexer.Indentation.Tabs(n2) :: is =>
        if (n == n2) (SysMLLexer.SEPARATOR,new IndentScanner(input.rest, indentStack),input.pos)
        else if (n > n2) (SysMLLexer.INDENT,new IndentScanner(input.rest, s::indentStack),input.pos)
        else if (indentStack.contains(s)) (SysMLLexer.DEDENT,new IndentScanner(input,is),input.pos)
        else (SysMLLexer.UNMATCHED_DEDENT(s),new IndentScanner(input.rest,indentStack),input.rest.pos)
      case (b@SysMLLexer.Indentation.Spaces(n2)) :: is =>
        (SysMLLexer.INCONSISTENT_INDENTATION(b, s),new IndentScanner(input.rest,indentStack),input.rest.pos)
    }
    case other => (other,new IndentScanner(input.rest,indentStack),input.pos)
  }

  override def source = input.source
}
