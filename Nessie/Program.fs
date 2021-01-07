// Learn more about F# at http://fsharp.org

open System

open Nessie

[<EntryPoint>]
let main argv =
    let text = @"
let length-of (list: List) = 
    if list empty? then 0 else length-of (tail-of list) + 1
" 
    
    let tokens, errors = Lex.lex text

    printfn "Tokens: %A" (tokens |> List.ofSeq |> List.map string)
    printfn "Errors: %A" errors
    0 // return an integer exit code
