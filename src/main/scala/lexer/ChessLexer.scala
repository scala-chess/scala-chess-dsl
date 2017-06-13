package lexer

import compiler.{ChessLexerError, Location}

import scala.util.parsing.combinator.RegexParsers

object ChessLexer extends RegexParsers{
  override def skipWhitespace= true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def apply(code: String): Either[ChessLexerError, List[ChessToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(ChessLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[ChessToken]] = {
    phrase(rep1(piece | action | with_ | colon | if_ | equals | comma | left_bracket | right_bracket | left_paren |
      right_paren | literal | identifier | id))
  }

  def literal: Parser[LITERAL] = positioned  {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]+".r ^^ {str => IDENTIFIER(str)}
  }

  def id: Parser[ID] = positioned {
    "[a-zA-Z]".r ^^ {str => ID(str)}
  }

  def piece = positioned {"piece" ^^ (_ => PIECE())}
  def action = positioned {"action" ^^ (_ => ACTION())}
  def with_ = positioned {"with" ^^ (_ => WITH())}
  def colon = positioned {":" ^^ (_ => COLON())}
  def if_ = positioned {"if" ^^ (_ => IF())}
  def equals = positioned {"=" ^^ (_ => EQUALS())}
  def comma = positioned {"," ^^ (_ => COMMA())}
  def left_bracket = positioned {"[" ^^ (_ => LEFT_BRACKET())}
  def right_bracket = positioned {"]" ^^ (_ => RIGHT_BRACKET())}
  def left_paren = positioned {"(" ^^ (_ => LEFT_PAREN())}
  def right_paren = positioned {")" ^^ (_ => RIGHT_PAREN())}

}

