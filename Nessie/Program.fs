// Learn more about F# at http://fsharp.org

open System

open Nessie
open Nessie.TypeChecker

[<EntryPoint>]
let main argv =
    let text = @"
let x = 2 in 
let y = x + 3 in
    y + x
" 
    
    let tokens, errors = Lex.lex text

    let ast = Parse.parse tokens

    let vars = Vars.empty |> Vars.push "+" (Type.Func (Type.Int, Type.Func(Type.Int, Type.Int)))
    let expr = Result.bind (TypeChecker.typeCheckAst vars) (ast |> Result.mapError (fun _ -> Unchecked.defaultof<TypeChecker.Error>))

    printfn "Tokens: %s" (tokens |> Seq.map string |> String.concat " ")
    printfn "Errors: %A" errors
    printfn "Ast: %A" (ast |> Result.map string |> Result.mapError string)
    printfn "Expr: %A" (expr |> Result.map string |> Result.mapError string)
    printfn "Type: %A" (expr |> Result.map (fun x -> x.Type |> string) |> Result.mapError string)
    0 // return an integer exit code
