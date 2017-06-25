import generator.ChessGenerator
import org.scalatest.{FlatSpec, Matchers}
import parser._

class ChessCompilerSpec extends FlatSpec with Matchers {
  val input =
    """
      | piece "Bishop"(B) with [
      |   Move:Line,
      |   Move:Pattern(test1 = "Test1", test2 = "Test2") if [Condition1, Condition2(conditionKey = "condition value")],
      |   "namedAction"
      | ]
      | action "namedAction" TestAction:TestPattern
      |
      | board
      | |B|_|_|
      | |_|_|_|
      | |b|_|_|
      |""".stripMargin.trim


  val generatedActionFile =
    """package model.generated
      |
      |import model.generated.GeneratedPieces._
      |import model.logic._
      |import model.logic.modifier._
      |import model.util.Patterns._
      |
      |object GeneratedActions {
      |  val __action0 = new Line with ToMove
      |  val __action1 = new Pattern(test1 = Test1, test2 = Test2) with Condition1 with Condition2(conditionKey = condition value) with ToMove
      |  val __action2 = new TestPattern with ToTestAction
      |}
      |""".stripMargin.trim

  val generatedPiecesFile =
    """package model.generated
      |
      |import model.generated.GeneratedActions._
      |import model.PieceUtil._
      |
      |object GeneratedPieces {
      |
      |  def getByName(name: String): GeneratedPiece =
      |    name match {
      |      case __BISHOP.name => __BISHOP
      |    }
      |
      |  val hero = __BISHOP
      |
      |  case object __BISHOP extends GeneratedPiece {
      |    val id = "B"
      |    val name = "Bishop"
      |    val logic = List(__action0, __action1, __action2)
      |  }
      |
      |}
      |""".stripMargin.trim

  val generatedBoardFile =
    """package model.generated
      |
      |import chess.api._
      |import model.generated.GeneratedPieces._
      |import model.util.Colors._
      |
      |object GeneratedBoard {
      |  def apply(): List[Either[Action, Config]] =
      |    Right(BoardSize(3, 3)) :: (actions map { action => Left(action) })
      |
      |  val actions =
      |    List(
      |    PutInitial((0,0), __BISHOP(black)),
      |    PutInitial((0,2), __BISHOP(white))
      |    )
      |
      |}""".stripMargin.trim

  "Chess compiler" should "successfully parse a valid chess setup" in {
    val generatedFiles = ChessCompiler(input).right.getOrElse(null)
    generatedFiles.actions shouldBe generatedActionFile
    generatedFiles.pieces shouldBe generatedPiecesFile
    generatedFiles.board shouldBe generatedBoardFile
  }
}
