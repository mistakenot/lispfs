module Types

type Value = 
    | Int of int
    | String of string
    | List of (Value list)
    | Function of (Value -> Value)

type Context = {
    Identifiers: Map<string, Value>
    Parent: Context option }

// (define s:string params:list<string> body:Function)

let (|StringList|NotStringList|) =
    let rec loop accum = function
        | String(s)::tail -> loop (accum @ [s]) tail
        | [] -> StringList accum
        | _ -> NotStringList
    function
    | List(values) -> loop [] values
    | _ ->  NotStringList

let (|FunctionName|Invalid|) = function
    | String(s) -> FunctionName s
    | _ -> Invalid "Function name must be a string"
   

let (|Function|Invalid|) = function
    | List(x::y::[body]) -> 
        match x with
        | FunctionName(name) ->
            match y with
            | StringList(parameters) -> 
                Function(name, parameters, body)
            | NotStringList -> Invalid "Parameter list must be strings."
        | Invalid(e) -> Invalid e        
    | _ -> Invalid "A valid function definition must follow define call."    

// let define context = function
//     | Function(name, items, body) -> 
//         {context with Identifiers = context.Identifiers.Add(name, )}
//     |