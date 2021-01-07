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

    printfn "Tokens: %A" (tokens |> List.ofSeq |> List.map string)
    printfn "Errors: %A" errors
    printfn "Ast: %A" ast
    0 // return an integer exit code
