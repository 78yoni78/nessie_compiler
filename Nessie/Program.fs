// Learn more about F# at http://fsharp.org

open System

open Nessie

[<EntryPoint>]
let main argv =
    let text = @"
let f = x: int -> y: int -> x in
    f 2 3
" 
    
    let tokens, errors = Lex.lex text
    printfn "Tokens: %s" (tokens |> Seq.map string |> String.concat " ")
    printfn "Errors: %A" errors

    let ast = Parse.parse tokens
    printfn "Ast: %A" (ast |> Result.map string |> Result.mapError string)

    let vars = [
        "int", Value.Type (Type.Int)
        "+", Value.Func (Function.External (Type.Int, Type.Int, "+"))
        "-", Value.Func (Function.External (Type.Int, Type.Int, "-"))
        "*", Value.Func (Function.External (Type.Int, Type.Int, "*"))
        "/", Value.Func (Function.External (Type.Int, Type.Int, "/"))
    ]
    let expr = Result.bind (TypeCheck.typeCheck vars) (ast |> Result.mapError (fun _ -> Unchecked.defaultof<TypeCheck.TypeError>))
    printfn "Expr: %A" (expr |> Result.map string |> Result.mapError string)
    printfn "Type: %A" (expr |> Result.map (fun x -> x.Type |> string) |> Result.mapError string)

    0 // return an integer exit code
