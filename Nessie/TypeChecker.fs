module rec Nessie.TypeChecker

open Result

[<RequireQualifiedAccess>]
type Error = 
    | UndefinedIdentifier of Token
    | NotAFunction of func: Expr
    | NoFunctions of func1: Expr * func2: Expr
    | ArgumentDoesntMatch of func: Expr * arg: Expr
    | AmbiguousApplication of Expr * Expr

[<Struct>]
type Vars = { Map: Map<string, list<int * Type>>; Count: int }

module Vars =
    let count {Count=count} = count

    let tryFind name {Map=map} = map.TryFind name |> Option.bind List.tryHead
    
    let push name varType {Map=map; Count=count} =
        let var = (count, varType)

        let map' = 
            match map.TryFind name with
            | None -> 
                Map.add name [ var ] map
            | Some lst -> 
                Map.add name (var :: lst) map
        
        { Map = map'; Count = count + 1 }

    let empty = { Map=Map.empty; Count=0 }

/// Return the argument and return of a function type. If not a function type, none.
let private (|Applicable|_|) = function
    | Type.Specific (Value.Func _ as func) -> 
        match func.OriginType with
        | Type.Func (arg, ret) -> Some (arg, ret)
        | _ -> None
    | Type.Func (arg, ret) ->
        Some (arg, ret)
    | _ -> None

let private valueOfToken token = 
    match Token.kind token with
    | TokenKind.Int i -> Value.Int i
    | _ -> invalidArg (nameof token) "Token was not a literal"

let typeCheckAst (vars: Vars) (ast: Ast): Result<Expr, Error> =
    match ast with
    | Ast.Literal token ->
        let value = valueOfToken token
        Ok (Expr.Literal (value, token))
    | Ast.Var token ->
        let (TokenKind.Identifier name) = token.Kind
        match Vars.tryFind name vars with
        | None -> Error(Error.UndefinedIdentifier token)
        | Some (varID, varType) -> Ok (Expr.Var(varID, varType, token))
    | Ast.Let (varToken, varAst, ret) ->
        result {
            let! varExpr = typeCheckAst vars varAst
            let varType = varExpr.Type
            let (TokenKind.Identifier varName) = varToken.Kind

            let vars' = Vars.push varName varType vars
            let! retExpr = typeCheckAst vars' ret
            return Expr.Let(varExpr, retExpr)
        }
    | Ast.LApply (funcAst, argAst)
    | Ast.RApply (argAst, funcAst) ->
        result {
            let! argExpr = typeCheckAst vars argAst
            let! funcExpr = typeCheckAst vars funcAst
            let argType, funcType = argExpr.Type, funcExpr.Type
            match funcType with 
            | Applicable (expectedArgType, _) ->
                if not (expectedArgType.SuperSet argType) then
                    return! Error (Error.ArgumentDoesntMatch(funcExpr, argExpr))
                else
                    match ast with
                    | Ast.LApply _ -> 
                        return Expr.LApply (funcExpr, argExpr)
                    | Ast.RApply _ -> 
                        return Expr.RApply (argExpr, funcExpr)
            | _ -> 
                return! Error (Error.NotAFunction funcExpr)
        }
    | Ast.Apply (a1, a2) ->
        result {
            let! e1 = typeCheckAst vars a1
            let! e2 = typeCheckAst vars a2
            match e1.Type, e2.Type with
            | Applicable _, Applicable _ -> return! Error (Error.AmbiguousApplication(e1, e2))
            | Applicable _, _ -> return! typeCheckAst vars (Ast.LApply(a1, a2))
            | _, Applicable _ -> return! typeCheckAst vars (Ast.RApply(a1, a2))
            | _ -> return! Error (Error.NoFunctions(e1, e2))
        }