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
    
    let rec private many' p acc =
        parse {
            let! maybe = p
            match maybe with
            | None -> return List.rev acc
            | Some x -> return! many' p (x :: acc)
        }
    let many p = many' p []
    let separated (sep: 's ParseAction) p = 
        parse {
            let! x1 = p
            match x1 with
            | None -> return None
            | Some x1 ->
                let! xs = many <| parse {
                    let! s = sep
                    let! xn = p
                    return Option.map (fun xn -> s, xn) xn
                }
                return Some (x1, xs)
        }

    let tryParse parser = 
        ParseAction (fun con -> 
            match parseWith parser con with
            | Ok (a, con') ->
                Ok (Ok a, con')
            | Error e -> 
                Ok (Error e, con))

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
            return Some e
        | TokenKind.Identifier _ ->
            do! advance
            return Some (Ast.Var current)
        | TokenKind.Int _ ->
            do! advance
            return Some (Ast.Literal current)
        | _ -> return None
    }

and private ``application expression`` =
    parse {
        let! maybe = 
            separated (parse {
                let! current = peek
                match Token.kind current with
                | TokenKind.LArrow -> 
                    do! advance 
                    return Ast.LApply
                | TokenKind.RArrow -> 
                    do! advance 
                    return Ast.RApply
                | _ -> return Ast.Apply
            }) atom
        match maybe with
        | None -> return None
        | Some (x1, xn) ->
            return List.fold (fun x (sep, y) -> sep (x, y)) x1 xn |> Some
    }

and private ``lambda expression start`` = 
    parse {
        let! varToken = expectIdentifier
        let! _ = expectEq TokenKind.Colon
        let! varType = expr
        let! _ = expectEq TokenKind.LongArrow
        return varToken, varType
    }

and private ``lambda expression end`` var  = 
    parse {
        let! body = expr
        return Ast.Lambda(var, body)
    }

and private expr =
    parse {
        let! lambdaStart = tryParse ``lambda expression start``
        match lambdaStart with 
        | Ok var -> 
            return! ``lambda expression end`` var
        | Error e -> 
            let! current = peek
            match Token.kind current with
            | TokenKind.Let -> 
                return! ``let declaration`` 
            | TokenKind.Identifier _ 
            | _ -> 
                let! e1 = ``application expression``
                match e1 with
                | None -> return! error (ParseError.Unexpected("expression", current))
                | Some e1 -> return e1
    }

let parse (tokens: Tokens) =
    parseWith expr { ParseContext.Tokens = tokens; Index = 0 }
    |> Result.map fst

