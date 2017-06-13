import compiler.ChessCompilationError
import lexer.ChessLexer
import parser.{ChessAST, ChessParser}

object ChessCompiler {
  def apply(code: String): Either[ChessCompilationError, ChessAST] = {
    for {
      tokens <- ChessLexer(code).right
      ast <- ChessParser(tokens).right
    } yield ast
  }
}