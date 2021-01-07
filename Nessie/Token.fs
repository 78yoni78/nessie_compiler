namespace Nessie

[<Struct; RequireQualifiedAccess>]
type TokenKind =
    | Identifer of s: string
    | Int of i: int
    | LParen
    | RParen
    | LBrack
    | RBrack
    | LBrace
    | RBrace
    | Colon
    | Let 
    | In
    | EOF
    override t.ToString() = 
        match t with
        | Identifer s -> s 
        | Int i -> string i
        | LParen -> "("
        | RParen -> ")"
        | LBrack -> "["
        | RBrack -> "]"
        | LBrace -> "{"
        | RBrace -> "}"
        | Colon -> ":"
        | Let -> "let"
        | In -> "in"
        | EOF -> "EOF"
    
[<Struct>]
type Token = 
    { Kind: TokenKind; Offset: int; Length: int }
    override t.ToString() = string t.Kind

module Token =
    let kind {Kind = t} = t
    