module Env

open Parser

type Reference = 
    // Regular Tree
    | Code of Tree
    // Tree factory, for implementing native functions
    //          (args -> result)
    | Native of (Tree -> Tree)

type Environment = string -> Tree option

let add (id: string) (tree: Tree) (env: Environment) = (fun s -> 
    if id = s then Some(tree) else env s)

let addNative (id: string) (func: Environment -> Tree) (env: Environment) = (fun s -> 
    if id = s then Some(func env) else env s)

let get (id: string) (env: Environment) = Some(Atom(Nil))

let getInt (id: string) (env: Environment) =
    match env id with
    | Some(Atom(Int i)) -> i
    | _ -> sprintf "Value %s is not an int or not found" id |> failwith

let rec foldArgs (argLabels: Tree) (argValues: Tree) (env: Environment) = 
    // failwith (sprintf "%O" argValues)
    match (argLabels, argValues) with
    // End of the argument list
    | Atom Nil, Atom Nil -> env
    | Atom Nil, _ -> failwith "Too many args."
    // Variable to add
    | Atom(Ref label), value -> add label value env
    | Atom(_), _ -> failwith "Arg list can only contain references."
    | Tree(argL, argR), Tree(valueL, valueR) -> 
        foldArgs argL valueL env 
        |> foldArgs argR valueR
    | t -> sprintf "Arg list can only contain references: %O" t |> failwith