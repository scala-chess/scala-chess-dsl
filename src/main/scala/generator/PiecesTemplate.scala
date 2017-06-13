package generator

object PiecesTemplate {

  def apply(pieces: Seq[GeneratedPieceEntry]): String = {
    val pieceObjectStrings = pieces.map(pieceObject) mkString "\n"
    val pieceMatcher = pieces map {
      "__" + _.name.toUpperCase
    } map { name => s"case $name.name => $name" } mkString "\n"
    val hero = pieces.head
    s"""
       |package model.generated
       |
       |import model.generated.GeneratedActions._
       |import model.PieceUtil._
       |
       |object GeneratedPieces {
       |
       |  def getByName(name: String): GeneratedPiece =
       |    name match {
       |      $pieceMatcher
       |    }
       |
       |  val hero = __${hero.name.toUpperCase()}
       |
       |$pieceObjectStrings
       |}
   """.stripMargin.trim
  }

  private def pieceObject(piece: GeneratedPieceEntry) =
    s"""  case object __${piece.name.toUpperCase()} extends GeneratedPiece {
       |    val id = "${piece.id}"
       |    val name = "${piece.name}"
       |    val logic = List(${piece.logic})
       |  }
       |""".stripMargin

}
