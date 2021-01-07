module Nessie.Lex

open System
open System.Text

[<Struct; RequireQualifiedAccess>]
type LexError = { Offset: int; Message: string }

module private Helper =

    type LexContext = { Str: string; Index: int ref }
    let (|LexContext|) {LexContext.Str=str; Index=i} = str, i 

    let inline inRange { LexContext.Str=str; Index=i } = 
        !i < str.Length
    
    let inline eof context = not << inRange

    let skip predicate (LexContext(str, i) as context) = 
        while inRange context && predicate str.[!i] do
            incr i

    let private sequence predicate (LexContext(str, i) as con) = 
        if not (predicate str.[!i]) then
            None
        else
            let i' = !i //  remember where we started 

            skip predicate con // move to end of int

            Some str.[i'..(!i - 1)]

    let (|Int|_|) = sequence Char.IsDigit >> Option.map int
    
    let private allowedInIdentifer = function
        | '(' | ')' | '[' | ']' | '{' | '}' | ':' -> false
        | c when Char.IsWhiteSpace c -> false
        | _ -> true

    let (|IdentifierOrKeyword|_|) = 
        sequence allowedInIdentifer >> Option.map (function
        | "let" -> TokenKind.Let
        | "in" -> TokenKind.In
        | s -> TokenKind.Identifier s)

    /// maps to the correct token type of a single character simple terminal
    let (|SimpleToken|_|) { LexContext.Str=str; Index=i } =
        match str.[!i] with
        | '(' -> Some TokenKind.LParen
        | ')' -> Some TokenKind.RParen
        | '[' -> Some TokenKind.LBrack
        | ']' -> Some TokenKind.RBrack
        | '{' -> Some TokenKind.LBrace
        | '}' -> Some TokenKind.RBrace
        | ':' -> Some TokenKind.Colon
        | _ -> None
        |> function
        | Some x -> incr i; Some x
        | None -> None 

open Helper
    
let lex (str: string): Token ResizeArray * LexError ResizeArray =
    let i = ref 0
    let context = { Str = str; Index = i }
    let errors = ResizeArray ()
    let tokens = ResizeArray ()

    skip Char.IsWhiteSpace context

    while inRange context do
        let i' = !i // remember where the token was started
        
        //  get the length and type of the current token (or None for failure)
        let maybeKind = 
            match context with
            | SimpleToken kind -> Some kind
            | Int num -> Some(TokenKind.Int num)
            | IdentifierOrKeyword kind -> Some kind
            | _ -> None

        //  Handle fail and success differently
        match maybeKind with
        | None ->
            let message = sprintf "Unexpected character '%c'" str.[i']
            let er = { LexError.Offset = i'; LexError.Message = message }
            
            //  report an error and try to keep parsing
            skip (not << Char.IsWhiteSpace) context             
            errors.Add er
            ()
        | Some kind ->
            let length = !i - i'
            let token = { Token.Kind = kind; Offset = i'; Length = length }

            tokens.Add token
            ()

        skip Char.IsWhiteSpace context

    tokens.Add {Token.Kind = TokenKind.EOF; Offset = !i; Length = 0}

    tokens, errors

