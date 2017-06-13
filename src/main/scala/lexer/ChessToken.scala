package lexer

import scala.util.parsing.input.Positional

sealed trait ChessToken extends Positional

case class LITERAL(str: String) extends ChessToken
case class IDENTIFIER(str: String) extends ChessToken
case class ID(str: String) extends ChessToken
case class PIECE() extends ChessToken
case class ACTION() extends ChessToken
case class WITH() extends ChessToken
case class COLON() extends ChessToken
case class IF() extends ChessToken
case class EQUALS() extends ChessToken
case class COMMA() extends ChessToken
case class LEFT_BRACKET() extends ChessToken
case class RIGHT_BRACKET() extends ChessToken
case class LEFT_PAREN() extends ChessToken
case class RIGHT_PAREN() extends ChessToken
case class PIPE() extends ChessToken
case class UNDERSCORE() extends ChessToken
