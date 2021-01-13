module Nessie.TypeCheck

open Result

[<RequireQualifiedAccess>]
type TypeError = 
    | UndefinedIdentifier of Token
    | NotAFunction of func: Expr
    | NoFunctions of func1: Expr * func2: Expr
    | ArgumentDoesntMatch of func: Expr * arg: Expr
    | AmbiguousApplication of Expr * Expr

module private Helper =
    [<Struct>]
    type TypeContext = { Map: Map<string, list<int * Type>>; Count: int }

    module TypeContext =
        let count {Count=count} = count
        
        let empty = { Map=Map.empty; Count=0 }

        let tryFind name { Map=map } = map.TryFind name |> Option.bind List.tryHead
    
        let push name varType { Map=map; Count=count } =
            let var = (count, varType)

            let map' = 
                match map.TryFind name with
                | None -> 
                    Map.add name [ var ] map
                | Some lst -> 
                    Map.add name (var :: lst) map
        
            { Map = map'; Count = count + 1 }

    /// Return the argument and return of a function type. If not a function type, none.
    let (|Applicable|_|) = function
        | Type.Specific (Value.Func _ as func) -> 
            match func.OriginType with
            | Type.Func (arg, ret) -> Some (arg, ret)
            | _ -> None
        | Type.Func (arg, ret) ->
            Some (arg, ret)
        | _ -> None
    
    /// get the value of a literal token (or throw)
    let literal token = 
        match Token.kind token with
        | TokenKind.Int i -> Value.Int i
        | _ -> invalidArg (nameof token) "Token was not a literal"

    /// get the name of an identifier token (or throw)
    let identifier token =
        match Token.kind token with
        | TokenKind.Identifier name -> name
        | _ -> invalidArg (nameof token) "Token was not an identifier"

    let find token context = 
        match TypeContext.tryFind (identifier token) context with
        | None -> Error(TypeError.UndefinedIdentifier token)
        | Some (varID, varType) -> Ok (Expr.Var(varID, varType, token))

    let apply (func: Expr, arg: Expr) = 
        let argType, funcType = arg.Type, func.Type
        match funcType with 
        | Applicable (expectedArgType, _) ->
            if not (expectedArgType.SuperSet argType) then
                Error (TypeError.ArgumentDoesntMatch(func, arg))
            else
                Ok ()
        | _ -> 
            Error (TypeError.NotAFunction func)

open Helper

let rec private typeCheckCon (con: TypeContext): Ast -> Result<Expr, TypeError> =
    function
    | Ast.Literal token -> Ok (Expr.Literal (literal token, token))
    | Ast.Var token -> find token con
    | Ast.Let (varToken, varAst, ret) ->
        result {
            //  type check the first expression
            let! varExpr = typeCheckCon con varAst
            //  type check the body (make a variable)
            let vars' = TypeContext.push (identifier varToken) varExpr.Type con
            let! retExpr = typeCheckCon vars' ret

            return Expr.Let(varToken, varExpr, retExpr)
        }
    | Ast.Lambda ((varToken, varAst), bodyAst) ->
        result {
            let! varExpr = typeCheckCon con varAst
            
            let con' = TypeContext.push (identifier varToken) varExpr.Type con
            let! bodyExpr = typeCheckCon con' bodyAst

            return Expr.Lambda((varToken, varExpr), bodyExpr)
        }
    | Ast.LApply (funcAst, argAst) ->
        result {
            let! funcExpr = typeCheckCon con funcAst
            let! argExpr = typeCheckCon con argAst
            do! apply (funcExpr, argExpr) //   make sure you can apply
            return Expr.LApply(funcExpr, argExpr)
        }
    | Ast.RApply (argAst, funcAst) ->
        result {
            let! argExpr = typeCheckCon con argAst
            let! funcExpr = typeCheckCon con funcAst
            do! apply (funcExpr, argExpr) //   make sure you can apply
            return Expr.RApply(argExpr, funcExpr)
        }
    | Ast.Apply (a1, a2) ->
        result {
            let! e1 = typeCheckCon con a1
            let! e2 = typeCheckCon con a2
            match e1.Type, e2.Type with
            | Applicable _, Applicable _ -> return! Error (TypeError.AmbiguousApplication(e1, e2))
            | Applicable _, _ -> 
                do! apply (e1, e2)
                return Expr.LApply(e1, e2)
            | _, Applicable _ ->                 
                do! apply (e2, e1)
                return Expr.RApply(e1, e2)
            | _ -> return! Error (TypeError.NoFunctions(e1, e2))
        }

let typeCheck (ast: Ast) =
    typeCheckCon TypeContext.empty ast
