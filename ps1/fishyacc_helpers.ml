let parse_code path =
 Parse.program Lex.lexer (Lexing.from_channel (open_in path))
