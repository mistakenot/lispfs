module Utils

type Result<'a> = Ok of 'a | Error of string

type ResultBuilder () = 
    member this.Bind(value, next) = 
        match value with
        | Ok(a) -> next a
        | Error(e) -> Error(e)

let result = ResultBuilder()