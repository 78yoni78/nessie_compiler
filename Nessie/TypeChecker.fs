module rec Nessie.TypeChecker

open Nessie

type Error = UndefinedIdentifier of string

[<Struct>]
type Vars = Vars of Map<string, list<int * Type>>

module Vars =
    let tryFind name (Vars map) = map.TryFind name |> Option.bind List.tryHead

    let push name (varID, varType) (Vars map) =
        let var = (varID, varType)

        match map.TryFind name with
        | None -> Map.add name [ var ] map |> Vars
        | Some lst -> Map.add name (var :: lst) map |> Vars

    let count (Vars map) = Map.count map

    let newID vars = count vars

let typeOf (expr: Expr): Type = failwithf "no"

let typeCheckAst (vars: Vars) (ast: Ast): Result<Expr, Error> =
    match ast with
    | Ast.Int i -> Ok <| Expr.Val(Value.Int i)
    | Ast.Var name ->
        match Vars.tryFind name vars with
        | None -> Error(UndefinedIdentifier name)
        | Some (varID, varType) -> Ok <| Expr.Var(varID, varType)
    | Ast.Let (varName, varAst, ret) ->
        typeCheckAst vars ast
        |> Result.bind
            (fun varExpr -> 
                let varType = typeOf varExpr
                Vars.push varName (Vars.newID vars, varType) vars
                |> typeCheckAst <| ret
                |> Result.map (fun retExpr -> 
                    Expr.Let(varExpr, retExpr)))
    | Ast.LApply(funcAst, argAst) | Ast.RApply(argAst, funcAst) ->
        //typeCheckAst vars argAst
        //|> Result.bind (fun argExpr ->
        //    typeCheckAst vars funcAst
        //    |> Result.bind (fun funcExpr ->
        //        let type))
        failwithf ""
