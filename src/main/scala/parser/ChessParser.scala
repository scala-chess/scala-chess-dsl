package parser

import compiler.{ChessParserError, Location}
import lexer._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object ChessParser extends Parsers {
  override type Elem = ChessToken

  class ChessTokenReader(tokens: Seq[ChessToken]) extends Reader[ChessToken] {
    override def first: ChessToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[ChessToken] = new ChessTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[ChessToken]): Either[ChessParserError, ChessAST] = {
    val reader = new ChessTokenReader(tokens)
    print(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(ChessParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }

  def program: Parser[ChessAST] = positioned {
    phrase(block)
  }

  def block: Parser[ChessAST] = positioned {
    rep1(statement) ^^ (statements => ChessAST(statements))
  }

  def statement: Parser[ChessStatement] = positioned {
    val actionAssignment = ACTION() ~ literal ~ actionDefinition ^^ {case _ ~ LITERAL(name) ~ definition => ActionAssignment(name, definition)}

    val board = (BOARD() ~  rank.+) ^^ {case _ ~ ranks  => Board(ranks)}

    val piece: Parser[ChessStatement] =
      PIECE() ~ literal ~ LEFT_PAREN() ~ id ~ RIGHT_PAREN() ~
        WITH() ~ LEFT_BRACKET() ~ rep1sep(action, COMMA()) ~ RIGHT_BRACKET() ^^ {
        case _ ~ LITERAL(pieceName) ~ _ ~ ID(pieceId) ~ _ ~ _ ~ _ ~ actions ~ _ => Piece(pieceName, pieceId, actions)
      }

    actionAssignment | piece | board

  }

  def rank: Parser[Rank] = positioned {
    PIPE() ~ rep1sep( square, PIPE() ) ~ PIPE() ^^ {case _  ~ squares ~ _ => Rank(squares)}
  }

  def square: Parser[Square] = positioned {
    (id | UNDERSCORE()) ^^ {
      case ID(id) => OccupiedSquare(id)
      case _ => EmptySquare()
    }
  }

  def action: Parser[Action] = positioned {
    val namedAction = literal ^^ { case LITERAL(name) => ActionNamed(name) }
    actionDefinition | namedAction
  }

  def actionDefinition: Parser[ActionDefinition] = positioned {
    identifier ~ COLON() ~ identifierWithOptions ~
      (IF() ~ LEFT_BRACKET() ~ rep1sep(identifierWithOptions, COMMA()) ~ RIGHT_BRACKET()).? ^^ {
      case IDENTIFIER(actionName) ~ _ ~ pattern ~ None =>
        ActionDefinition(actionName, pattern, Seq())
      case IDENTIFIER(actionName) ~ _ ~ pattern ~ Some(_ ~ _ ~ conditions ~ _) =>
        ActionDefinition(actionName, pattern, conditions)
    }
  }

  def identifierWithOptions: Parser[Identifier] = positioned {
    val with_ = identifier ~ LEFT_PAREN() ~ rep1sep(keyValue, COMMA()) ~ RIGHT_PAREN() ^^ { case IDENTIFIER(name) ~ _ ~ options ~ _ => IdentifierWithOptions(name, options) }
    val without = identifier ^^ { case IDENTIFIER(name) => IdentifierSingle(name) }

    with_ | without
  }

  def keyValue: Parser[KeyValue] = positioned {
    identifier ~ EQUALS() ~ literal ^^ { case IDENTIFIER(name) ~ _ ~ LITERAL(value) => KeyValue(name, value) }
  }

  def id: Parser[ID] = positioned {
    accept("id", { case id@ID(_) => id })
  }


  def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case identifier@IDENTIFIER(_) => identifier })
  }

  def literal: Parser[LITERAL] = positioned {
    accept("string literal", { case lit@LITERAL(_) => lit })
  }


}
