// Learn more about F# at http://fsharp.org

open System

open Nessie
open Nessie.TypeCheck

[<EntryPoint>]
let main argv =
    let text = @"
let x = 2 in 
let y = x + 3 in
    y + x
" 
    
    let tokens, errors = Lex.lex text

    let ast = Parse.parse tokens

    let expr = Result.bind TypeCheck.typeCheck (ast |> Result.mapError (fun _ -> Unchecked.defaultof<TypeCheck.TypeError>))

    printfn "Tokens: %s" (tokens |> Seq.map string |> String.concat " ")
    printfn "Errors: %A" errors
    printfn "Ast: %A" (ast |> Result.map string |> Result.mapError string)
    printfn "Expr: %A" (expr |> Result.map string |> Result.mapError string)
    printfn "Type: %A" (expr |> Result.map (fun x -> x.Type |> string) |> Result.mapError string)
    0 // return an integer exit code
