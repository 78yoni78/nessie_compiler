namespace Nessie

[<RequireQualifiedAccess>]
type Ast = 
    /// A literal (1, "hello")
    | Literal of Token
    /// A reference to a variable binding by name
    | Var of Token
    //  A variable binding
    | Let of Token * Ast * ret: Ast
    | Lambda of (Token * Ast) * Ast
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Ast * func: Ast
    /// Apply arg to func (but from the left to right)
    | LApply of func: Ast * arg: Ast
    /// Ambiguous application
    | Apply of Ast * Ast
    override t.ToString() =
        match t with
        | Literal token 
        | Var token
            -> string token
        | Let (var, body, retn) -> 
            sprintf "let %O = %O in %O" var body retn
        | Lambda ((var, typ), retn) -> 
            sprintf "(%O: %O -> %O)" var typ retn
        | RApply (arg, func) -> sprintf "(%O > %O)" arg func
        | LApply (func, arg) -> sprintf "(%O < %O)" func arg
        | Apply (left, right) -> sprintf "(%O %O)" left right
