package lexer


case class ChessLexerError(location: Location, msg: String)

case class Location( line: Int, column: Int) {
  override def toString: String = s"$line:$column"
}