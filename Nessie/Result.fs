module Result

type ResultBuilder() =
    member t.Return x = Ok x
    member t.ReturnFrom (x: Result<_, _>) = x
    member t.Bind(x, f) = Result.bind f x

let result = ResultBuilder()
