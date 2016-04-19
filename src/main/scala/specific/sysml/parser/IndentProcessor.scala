package specific.sysml.parser


import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

/**
  * Created by martin on 19.04.16.
  */
class IndentScanner(private var input: Reader[Lexer.Token], indentStack: List[Lexer.Indentation] = Nil) extends Reader[Lexer.Token] {
  def atEnd = input.atEnd && indentStack.isEmpty

  lazy val (first: Lexer.Token, rest: Reader[Lexer.Token], pos: Position) = if (input.atEnd) indentStack match {
    case i :: is => (Lexer.DEDENT,new IndentScanner(input,is),input.pos)
    case Nil => (Lexer.EOF,this,input.pos)
  } else input.first match {
    case Lexer.Indentation.None => indentStack match {
      case Nil => (Lexer.SEPARATOR,new IndentScanner(input.rest,indentStack),input.pos)
      case i :: is => (Lexer.DEDENT,new IndentScanner(input,is),input.pos)
    }
    case s@Lexer.Indentation.Spaces(n) => indentStack match {
      case Nil => (Lexer.INDENT,new IndentScanner(input.rest, s::Nil),input.pos)
      case Lexer.Indentation.Spaces(n2) :: is =>
        if (n == n2) (Lexer.SEPARATOR,new IndentScanner(input.rest, indentStack),input.pos)
        else if (n > n2) (Lexer.INDENT,new IndentScanner(input.rest, s::indentStack),input.rest.pos)
        else if (indentStack.contains(s)) (Lexer.DEDENT,new IndentScanner(input,is),input.rest.pos)
        else (Lexer.UNMATCHED_DEDENT(s),new IndentScanner(input.rest,indentStack),input.rest.pos)
      case Lexer.Indentation.Tabs(n2) :: is =>
        (Lexer.ErrorToken(s"opened indentation with $n spaces but continued with $n2 tabs"))
    }
    case s@Lexer.Indentation.Tabs(n) => indentStack match {
      case Nil => (Lexer.INDENT,new IndentScanner(input.rest, s::Nil),input.pos)
      case Lexer.Indentation.Tabs(n2) :: is =>
        if (n == n2) (Lexer.SEPARATOR,new IndentScanner(input.rest, indentStack),input.pos)
        else if (n > n2) (Lexer.INDENT,new IndentScanner(input.rest, s::indentStack),input.pos)
        else if (indentStack.contains(s)) (Lexer.DEDENT,new IndentScanner(input,is),input.pos)
        else (Lexer.UNMATCHED_DEDENT(s),new IndentScanner(input.rest,indentStack),input.rest.pos)
      case Lexer.Indentation.Spaces(n2) :: is =>
        (Lexer.ErrorToken(s"opened indentation with $n spaces but continued with $n2 tabs"))
    }
    case other => (other,new IndentScanner(input.rest,indentStack),input.pos)
  }

  override def source = input.source
}
