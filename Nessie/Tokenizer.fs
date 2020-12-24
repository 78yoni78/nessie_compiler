module Nessie.Tokenizer

open Nessie


type Token =
    | Identifer of string
    | IntLiteral of int
    | LeftParen | RightParen
    | LeftBrace | RightBrace
    | LeftCurl | RightCurl