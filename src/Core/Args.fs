module Args

// Helper functions for arguments.

open Abstractions

let str id args = 
    match args id with
    | Atom(Str(s)) -> s
    | _ -> sprintf "Argument %s is not a string." id |> failwith

let int id args = 
    match args id with
    | Atom(Int(i)) -> i
    | _ -> sprintf "Argument %s is not an integer." id |> failwith

let bool id args = 
    match args id with
    | Atom(Bool(b)) -> b
    | _ -> sprintf "Argument %s is not a boolean." id |> failwith

let ref id args = 
    match args id with
    | Atom(Ref(r)) -> r
    | _ -> sprintf "Argument %s is not a reference." id |> failwith