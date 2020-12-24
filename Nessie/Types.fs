[<AutoOpen>]
module Nessie.Types

/// A tree to be parsed from the source code
[<RequireQualifiedAccess>]
type Ast =
    /// A constant int value
    | Int of int
    (*
    /// A constant number value
    | Num of float
    /// A constant string value
    | Str of string
    /// Create a tuple
    | Tuple of Ast list
    /// Create a lambda
    | LambdaExpr of (string * Ast) * Ast
    /// Convert the type
    | TypeConvertion of Ast * Ast
    *)
    /// A variable term
    | Var of string
    //  A variable binding
    | Let of string * Ast * ret: Ast
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Ast * func: Ast
    /// Apply arg to func (but from the left to right)
    | LApply of func: Ast * arg: Ast
    /// Ambiguous application
    | Apply of Ast * Ast


/// A function
[<Struct>]
type Function =
    { ArgType: Type
      Body: Expr
      Name: string option }

and [<RequireQualifiedAccess>] Value =
    /// A value representing a type of values
    | Type of Type
    /// An int value
    | Int of int
    (*
    /// A floating-point value
    | Num of float
    /// A string value
    | Str of string
    /// A function value
    | Func of Function
    /// A tuple value containing a list of values
    | Tuple of Value list
    *)

/// A type is a set of values, sometimes represented by a predicate (which can be compiled)
and [<RequireQualifiedAccess>] Type =
    /// A type containing all values
    | Any
    /// Contains all int values
    | Int
    (*
    /// Contains all num values
    | Num
    /// Contains all str values
    | Str
    //  | Forall of string * Type
    //  | Var of string
    /// A function from the first type to the second
    | Func of Type * Type
    /// Contains tuples of a specific length with data matching the given types
    | NTuple of Type list
    /// Contains tuples of any length where the all the data matches the given type
    | XTuple of Type
    *)
    /// Contains only a single value
    | Specific of Value
    /// The sum of many subtypes (Contains all values contained by the subtypes)
    | Union of Type list

and [<RequireQualifiedAccess>] Expr =
    /// A constant value
    | Val of Value
    /// Load the value of a variable
    | Var of int * Type
    (*
    /// Create a tuple
    | Tuple of Expr list
    /// Create a lambda
    | Lambda of Function * closureVariables: int Set
    /// Convert the type
    | TypeConvertion of Expr * Type
    *)
    /// A let in expression
    | Let of Expr * Expr
    /// Apply arg to func (but from the right to left)
    | RApply of arg: Expr * func: Expr
    /// Apply arg to func (but from the left to right)
    | LApply of func: Expr * arg: Expr
