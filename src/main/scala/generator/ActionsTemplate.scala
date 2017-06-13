package generator

object ActionsTemplate {

  def apply(actions: Seq[GeneratedActionEntry]): String = {
    val actionStrings = actions map actionToString mkString "\n"
    s"""
       |package model.generated
       |
       |import model.generated.GeneratedPieces._
       |import model.logic._
       |import model.logic.modifier._
       |import model.util.Patterns._
       |
       |object GeneratedActions {
       |$actionStrings
       |}
   """.stripMargin.trim
  }

  private def actionToString(assignment: GeneratedActionEntry) = s"  val __action${assignment.id} = ${assignment.value}"

}
