namespace Nessie

[<Struct>]
type Function = { ArgType: Type; Body: Expr; Name: string option }

and [<RequireQualifiedAccess>] Type =
    | Specific of Value
    | Int 
    | Func of Type * Type

and [<RequireQualifiedAccess>] Value =
    | Type of Type
    | Int of int
    | Func of Function

and [<RequireQualifiedAccess>] Expr =
    | Literal of Value * Token
    | Var of int * Type * Token
    /// A let in expression
    | Let of Expr * Expr
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Expr * func: Expr
    /// Apply arg to func (but from the left to right)
    | LApply of func: Expr * arg: Expr
