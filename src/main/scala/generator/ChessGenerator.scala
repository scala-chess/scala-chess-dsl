package generator

import parser._
import TemplateHelper._

object ChessGenerator {

  def apply(chessAst: ChessAST) = {
    val ast = substituteNamedActions(chessAst)
    val uniqueActions = ast.statements.collect {
      case piece: Piece => piece.actions
    }.flatten.distinct.zipWithIndex.toMap

    val actionEntries = uniqueActions.toList map {
      case (action: ActionDefinition, id) => GeneratedActionEntry(id, action.template)
    }

    val pieceEntries = ast.statements.collect {
      case piece: Piece =>
        GeneratedPieceEntry(piece.id, piece.name, mapPieceActions(piece.actions, uniqueActions))
    }

    val pieceById = ast.statements.collect {
      case piece: Piece => piece.id.toUpperCase -> piece.name
    } toMap

    val ranks = ast.statements collectFirst {
      case Board(ranks) => ranks
    } getOrElse Seq()

    val boardEntries = ranks.map(_.squares).zipWithIndex.flatMap {
      case (squares, y) => squares.map(square => (square, y)).zipWithIndex
    } collect {
      case ((OccupiedSquare(pieceId), y), x) =>
        GeneratedBoardEntry((x, y), pieceById(pieceId.toUpperCase), pieceId)
    }


    val boardSize = (ranks.head.squares.size, ranks.size)

    GeneratedFiles(
      actions = ActionsTemplate(actionEntries),
      pieces = PiecesTemplate(pieceEntries),
      board = BoardTemplate(boardSize, boardEntries),
      config = ""
    )

  }

  def mapPieceActions(actions: Seq[Action], uniqueActions: Map[Action, Int]): String =
    actions map {
      uniqueActions(_)
    } map { id => s"__action$id" } mkString ", "


  def substituteNamedActions(ast: ChessAST): ChessAST = {
    val namedActions = ast.statements.collect {
      case a: ActionAssignment => a.name -> a.action
    } toMap

    val statements = ast.statements.flatMap {
      case a: ActionAssignment => None
      case p: Piece => Some(substituteNamedActions(p, namedActions))
      case other => Some(other)
    }

    ast.copy(statements = statements)
  }

  def substituteNamedActions(piece: Piece, namedActions: Map[String, ActionDefinition]): Piece = {
    val actions = piece.actions.map {
      case definition: ActionDefinition => definition
      case named: ActionNamed => namedActions(named.name)
    }
    piece.copy(actions = actions)
  }

}
