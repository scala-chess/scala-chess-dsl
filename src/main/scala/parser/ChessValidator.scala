package parser

import compiler.{ChessParserError, ChessValidationError, ChessValidationErrors, Location}
import lexer.ChessToken
import parser.ChessParser.{ChessTokenReader, program}

import scala.util.parsing.input.{Position, Positional}

object ChessValidator {

  def apply(ast: ChessAST): Either[ChessValidationErrors, ChessAST] = {

    List(
      onlyOneBoard _,
      uniquePieceNames _,
      uniqueActionName _,
      uniquePieceID _
    ) map {
      _ (ast)
    } collect {
      case Some(error) => error
    } flatten match {
      case Nil => Right(ast)
      case errors => Left(ChessValidationErrors(errors))
    }
  }

  def onlyOneBoard(ast: ChessAST): Option[Iterable[ChessValidationError]] =
    ast.statements collect { case b: Board => b } match {
      case Nil => Some(List(ChessValidationError(Location(0, 0), "No board specified.")))
      case _ :: Nil => None
      case first :: second :: tail => Some(List(ChessValidationError(loc(second), s"There is already another board specified at ${loc(first)}.")))
    }

  def uniquePieceNames(ast: ChessAST): Option[Iterable[ChessValidationError]] =
    ast.statements collect { case p: Piece => p
    } groupBy {
      p => p.name.toLowerCase
    } filter {
      _._2.size > 1
    } flatMap {
      duplicates =>
        duplicates._2.drop(1).map {
          p => ChessValidationError(loc(p), s"A piece with the name '${duplicates._1}' was already defined.")
        }
    } match {
      case Nil => None
      case errors => Some(errors)
    }

  def uniquePieceID(ast: ChessAST): Option[Iterable[ChessValidationError]] =
    ast.statements collect { case p: Piece => p
    } groupBy {
      p => p.id.toLowerCase
    } filter {
      _._2.size > 1
    } flatMap {
      duplicates =>
        duplicates._2.drop(1).map {
          p => ChessValidationError(loc(p), s"A piece with the id '${duplicates._1}' was already defined.")
        }
    } match {
      case Nil => None
      case errors => Some(errors)
    }

  def uniqueActionName(ast: ChessAST): Option[Iterable[ChessValidationError]] =
    ast.statements collect { case a: ActionAssignment => a
    } groupBy {
      a => a.name.toLowerCase
    } filter {
      _._2.size > 1
    } flatMap {
      duplicates =>
        duplicates._2.drop(1).map {
          p => ChessValidationError(loc(p), s"An action with the name '${duplicates._1}' was already defined.")
        }
    } match {
      case Nil => None
      case errors => Some(errors)
    }

  def loc(positional: Positional) = Location(positional.pos.line, positional.pos.column)

}
