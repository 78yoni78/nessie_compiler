namespace Nessie

[<RequireQualifiedAccess>]
type Ast = 
    /// A literal (1, "hello")
    | Literal of Token
    /// A reference to a variable binding by name
    | Var of Token
    //  A variable binding
    | Let of Token * Ast * ret: Ast
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Ast * func: Ast
    /// Apply arg to func (but from the left to right)
    | LApply of func: Ast * arg: Ast
    /// Ambiguous application
    | Apply of Ast * Ast
