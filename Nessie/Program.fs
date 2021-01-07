// Learn more about F# at http://fsharp.org

open System

open Nessie

[<EntryPoint>]
let main argv =
    let text = @"
let x = 2 in 
    x + 4
" 
    
    let tokens, errors = Lex.lex text

    let ast = Parse.parse tokens

    printfn "Tokens: %s" (tokens |> Seq.map string |> String.concat " ")
    printfn "Errors: %A" errors
    printfn "Ast: %A" (ast |> Result.map string |> Result.mapError string)
    0 // return an integer exit code
