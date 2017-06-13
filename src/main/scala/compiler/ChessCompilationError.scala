package compiler


sealed trait ChessCompilationError

case class ChessLexerError(location: Location, msg: String) extends ChessCompilationError
case class ChessParserError(location: Location, msg: String) extends ChessCompilationError

case class Location(line: Int, column: Int) {
  override def toString: String = s"$line:$column"
}
