import compiler.{ChessCompilationError, GeneratedFiles}
import generator.ChessGenerator
import lexer.ChessLexer
import parser.{ChessAST, ChessParser, ChessValidator}

object ChessCompiler {

  def apply(code: String): Either[ChessCompilationError, GeneratedFiles] =
    for {
      tokens <- ChessLexer(code).right
      ast <- ChessParser(tokens).right
      validatedAst <- ChessValidator(ast).right
      generatedFiles <- Right(ChessGenerator(validatedAst)).right
    } yield generatedFiles

}