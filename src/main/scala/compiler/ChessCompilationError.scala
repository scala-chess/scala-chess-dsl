package compiler


sealed trait ChessCompilationError

case class ChessLexerError(location: Location, msg: String) extends ChessCompilationError
case class ChessParserError(location: Location, msg: String) extends ChessCompilationError
case class ChessValidationErrors(errors: List[ChessValidationError]) extends ChessCompilationError {
  override def toString: String = errors.map(_.toString) mkString "\n"
}

case class ChessValidationError(location: Location, msg: String)

case class Location(line: Int, column: Int) {
  override def toString: String = s"$line:$column"
}
