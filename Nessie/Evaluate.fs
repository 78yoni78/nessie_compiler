module Nessie.Evaluate

open Result

module private Helper =
    [<Struct>]
    type EvaluationContext = EvaluationContext of Map<int, Value>

    let push value (EvaluationContext map) = EvaluationContext (map.Add (map.Count, value))

    let tryFind varId (EvaluationContext map) = map.TryFind varId

open Helper

[<RequireQualifiedAccess>]
type EvaluateError = 
    | UndefinedVariable of Token
    | NotAType of Value
    | NotAFunc of Value
    | In of Function * EvaluateError

let rec private evaluate' (con: EvaluationContext) = function
    | Expr.Literal(v, _) -> Ok v
    | Expr.Var(i, _, token) -> 
        match tryFind i con with
        | Some v -> Ok v
        | None -> Error (EvaluateError.UndefinedVariable token)
    | Expr.Lambda ((_, argType), bodyExpr) ->
        Ok <| Value.Func (Function.Lambda (argType, bodyExpr, None))
    | Expr.Let (_, varExpr, retExpr) ->
        result {
            let! varValue = evaluate' con varExpr
            let con' = push varValue con
            return! evaluate' con' retExpr
        }
    | Expr.LApply (funcExpr, argExpr) | Expr.RApply(argExpr, funcExpr) ->
        result {
            let! func = 
                evaluate' con funcExpr
                |> bind (function
                    | Value.Func func -> Ok func
                    | x -> Error (EvaluateError.NotAFunc x))
            let! argValue = evaluate' con argExpr
            match func with
            | Function.Lambda(_, bodyExpr, _) ->
                let con' = push argValue con
                return! 
                    evaluate' con' bodyExpr
                    |> mapError (fun e -> EvaluateError.In(func, e))
        }
        
let evaluate (vars: Map<int, Value>) =
    evaluate' (EvaluationContext vars)
