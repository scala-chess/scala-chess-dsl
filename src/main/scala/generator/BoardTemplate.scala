

package generator

object BoardTemplate {

  def apply(size: (Int, Int), entries: Seq[GeneratedBoardEntry]): String = {
    val entryStrings = entries map entryToString mkString ",\n"
    s"""
       |package model.generated
       |
       |import chess.api._
       |import model.generated.GeneratedPieces._
       |import model.util.Colors._
       |
       |object GeneratedBoard {
       |  def apply(): List[Either[Action, Config]] =
       |    Right(BoardSize(${size._1}, ${size._2})) :: (actions map { action => Left(action) })
       |
       |  val actions =
       |    List(
       |$entryStrings
       |    )
       |
       |}
       |""".stripMargin.trim
  }

  private def entryToString(entry: GeneratedBoardEntry) = s"    PutInitial(${entry.position}, __${entry.pieceName.toUpperCase}(${color(entry)}))"

  private def color(entry: GeneratedBoardEntry) = if(entry.pieceId.charAt(0).isUpper) "black" else "white"

}
