module Env

open Abstractions
open Parser

let add (id: string) (value: Variable) (env: Environment) = 
    let next: Environment = (fun s -> if id = s then Some(value) else env s)
    next

let get (env: Environment) (id: string) = 
    match env id with 
    | Some(v) -> v
    | None -> sprintf "Identifier %s not found." id |> failwith

let rec foldArgs (argLabels: Tree) (argValues: Tree) (env: Environment) = 
    // failwith (sprintf "%O" argValues)
    match (argLabels, argValues) with
    // End of the argument list
    | Atom Nil, Atom Nil -> env
    | Atom Nil, _ -> failwith "Too many args."
    // Variable to add
    | Atom(Ref label), value -> add label (Variable.Value value) env
    | Atom(_), _ -> failwith "Arg list can only contain references."
    | Tree(argL, argR), Tree(valueL, valueR) -> 
        foldArgs argL valueL env 
        |> foldArgs argR valueR
    | t -> sprintf "Arg list can only contain references: %O" t |> failwith

let toArgs (env: Environment) = (fun id -> 
    match get env id with 
    | Value(t) -> t
    | _ -> failwith "Not a value." )