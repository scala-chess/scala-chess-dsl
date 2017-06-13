package parser

import java.util.Optional

import scala.util.parsing.input.Positional

case class ChessAST(statements: Seq[ChessStatement]) extends Positional

sealed trait ChessStatement extends Positional
case class Piece(name: String, id: String, actions: Seq[Action]) extends ChessStatement
case class ActionAssignment(name: String, action: ActionDefinition) extends ChessStatement
case class Board(ranks: Seq[Rank]) extends ChessStatement

sealed trait Action extends Positional
case class ActionNamed(name: String) extends Action
case class  ActionDefinition(actionName: String, pattern: Identifier, conditions: Seq[Identifier]) extends Action

sealed trait Identifier extends Positional
case class IdentifierSingle(identifier: String) extends Identifier
case class IdentifierWithOptions(identifier: String, options: Seq[KeyValue]) extends Identifier

case class KeyValue(name: String, value: String) extends Positional

case class Rank(squares: Seq[Square]) extends Positional

sealed trait Square extends Positional
case class EmptySquare() extends Square
case class OccupiedSquare(pieceId: String) extends Square