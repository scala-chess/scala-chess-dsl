import org.scalatest.{FlatSpec, Matchers}
import parser._

class ChessCompilerSpec extends FlatSpec with Matchers {
  val validCode =
    """
      |piece "Bishop"(B) with [
      |   Move:Line,
      |   Move:Pattern(test1 = "Test1", test2 = "Test2") if [Condition1, Condition2(conditionKey = "condition value")],
      |   "namedAction"
      | ]
      | action "namedAction" TestAction:TestPattern
    """.stripMargin.trim

  val ast =
    ChessAST(
      List(
        Piece("Bishop", "B",
          List(
            ActionDefinition("Move", IdentifierSingle("Line"), Seq()),
            ActionDefinition(
              "Move",
              IdentifierWithOptions( "Pattern", List(KeyValue("test1", "Test1"), KeyValue("test2", "Test2"))),
              List(IdentifierSingle("Condition1"), IdentifierWithOptions("Condition2", List(KeyValue("conditionKey", "condition value"))))),
            ActionNamed("namedAction")
          )
        ),
        ActionAssignment("namedAction", ActionDefinition("TestAction", IdentifierSingle("TestPattern"), Seq()))
      )
    )

  "Chess compiler" should "successfully parse a valid chess setup" in {
    ChessCompiler(validCode) shouldBe Right(ast)
  }
}
