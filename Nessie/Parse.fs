module Nessie.Parse

open System.Collections.Generic

type Tokens = IReadOnlyList<Token>

[<RequireQualifiedAccess>]
type ParseError =
    | Unexpected of expceted: string * actual: Token

module private Helper =
    type ParseContext = { Tokens: Tokens; Index: int }

    [<Struct>]
    type 'T ParseAction = ParseAction of (ParseContext -> Result<'T * ParseContext, ParseError>)

    let parseWith (ParseAction f) = f 

    let bind (f: 'T -> 'U ParseAction) (x: 'T ParseAction) =
        ParseAction (fun con ->
            parseWith x con
            |> Result.bind (fun (t, con) ->
                parseWith (f t) con))

    let retn x = ParseAction (fun con -> Ok (x, con))

    let error x = ParseAction (fun con -> Error x)

    type ParseBuilder() =
        member t.Bind (x, f) =
            bind f x
        member t.Return x = retn x
        member t.ReturnFrom (x: 'a ParseAction) = x

    let parse = ParseBuilder()

    let peek = ParseAction (fun con -> Ok (con.Tokens.[con.Index], con)) 
    let advanceBy amount =ParseAction(fun con -> Ok ((), { con with Index=con.Index+amount }))
    let advance = advanceBy 1
    let moveBack = advanceBy -1
    let pop =
        parse {
            let! t = peek
            do! advance
            return t
        }
    let expect (description: string) predicate =
        parse {
            let! token = peek
            if predicate token.Kind then
                do! advance
                return token
            else 
                return! error (ParseError.Unexpected(description, token))
        }
            
    let expectEq tokenType = expect (string tokenType) ((=) tokenType)
    let expectIdentifier = expect "identifier" (function TokenKind.Identifier _ -> true | _ -> false)
    
open Helper

let rec private ``let declaration`` =
    parse {
        let! _ = expectEq TokenKind.Let 
        let! iden = expectIdentifier
        let! _ = expectEq (TokenKind.Identifier "=") 
        let! body = expr
        let! _ = expectEq TokenKind.In
        let! retn = expr
        return Ast.Let (iden, body, retn)

    }

and private atom =
    parse {
        let! current = peek
        match Token.kind current with
        | TokenKind.LParen -> 
            do! advance
            let! e = expr
            let! _ = expectEq TokenKind.LParen
            return e
        | TokenKind.Identifier _ ->
            do! advance
            return Ast.Var current
        | TokenKind.Int _ ->
            do! advance
            return Ast.Literal current
        | _ -> return! error (ParseError.Unexpected("atom", current))
    }

and private expr =
    parse {
        let! current = peek
        match Token.kind current with
        | TokenKind.Let -> 
            return! ``let declaration`` 
        | _ -> 
            return! atom
    }

let parse (tokens: Tokens) =
    parseWith expr { ParseContext.Tokens = tokens; Index = 0 }
    |> Result.map fst

