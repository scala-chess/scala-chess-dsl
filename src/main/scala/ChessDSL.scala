import java.io.PrintWriter
import sys.process._


object ChessDSL extends App {

  override def main(args: Array[String]): Unit = {
    //    val fileName = args(1)
    val fileName = "example.chess"
    val source = scala.io.Source.fromFile(fileName)
    val content = try source.mkString finally source.close()
    ChessCompiler(content) match {
      case Left(error) => print(s"Failure:\n$error")
      case Right(files) =>
        new PrintWriter("GeneratedActions.scala") {
          write(files.actions)
          close
        }
        new PrintWriter("GeneratedPieces.scala") {
          write(files.pieces)
          close
        }
        new PrintWriter("GeneratedBoard.scala") {
          write(files.board)
          close
        }

        "copy.bat" !

        print("Successful")
    }
  }
}
