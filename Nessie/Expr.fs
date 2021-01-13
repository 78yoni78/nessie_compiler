namespace Nessie

[<Struct>]
type Function = 
    { ArgType: Type; Body: Expr; Name: string option }
    member t.RetType = t.Body.Type

and [<RequireQualifiedAccess>] Type =
    | Specific of Value
    | Type 
    | Int 
    | Func of arg: Type * ret: Type
    member t.Contains value = 
        match t, value with
        | Specific otherValue, value -> otherValue = value
        | Type, Value.Type _ -> true
        | Int, Value.Int _ -> true
        | Func (arg, ret), Value.Func func -> 
            ret.SuperSet func.RetType && func.ArgType.SuperSet arg
        | _ -> false
    member superType.SuperSet subType =
        match superType, subType with
        | _ when superType = subType -> true
        | _, Specific value -> superType.Contains value
        | Func(arg1, ret1), Func(arg2, ret2) -> 
            ret1.SuperSet ret2 && arg2.SuperSet arg1
        | _ -> false

and [<RequireQualifiedAccess>] Value =
    | Type of Type
    | Int of int
    | Func of Function
    /// Get the supertype of a specifc type of this value
    /// ie: an int will return Type.Int, a type will return Type.Type and so on
    member t.OriginType = 
        match t with
        | Type _ -> Type.Type
        | Int _ -> Type.Int
        | Func func -> Type.Func (func.ArgType, func.RetType)

and [<RequireQualifiedAccess>] Expr =
    | Literal of Value * Token
    | Var of int * Type * Token
    /// A let in expression
    | Let of Token * Expr * Expr
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Expr * func: Expr
    /// Apply arg to func (but from the left to right)
    | LApply of func: Expr * arg: Expr
    member expr.Type = 
        match expr with
        | Literal (value, _) -> Type.Specific value
        | Var (_, t, _) -> t
        | Let (_, _, ret) -> ret.Type
        | RApply (_, func)
        | LApply (func, _) ->
            match func.Type with
            | Type.Func (_, retType) -> retType
            | _ -> failwithf "%O is a malformed expression tree" expr