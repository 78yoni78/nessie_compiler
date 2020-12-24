module Nessie.Tokenizer

open Nessie


[<Struct>]
type TokenType =
    | Identifer
    | IntLiteral of i: int
    | LeftParen | RightParen
    | LeftBrace | RightBrace
    | LeftCurl | RightCurl
    | EOF

[<Struct>]
type Token = { Type: TokenType; Str: string; Offset: int }
