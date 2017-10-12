module Libs

open Abstractions
open Args
open Env

// Standard library.

let identity: Function =
    let labels = Atom (Ref "x")
    let body = fun a -> a "x"
    (labels, body)

let addInt: Function = 
    let labels = 
        Tree(
            Atom (Ref "x"),
            Tree (
                Atom (Ref "y"),
                Atom (Nil)))
    let body = fun e -> 
        Atom (
            Int(Args.int "x" e + Args.int "y" e))
    (labels, body)

let all = [
    ("id", identity)
    ("+", addInt)]

let add (env: Environment) = 
    List.fold (fun e (label, body) -> Env.add label (Variable.Function body) e) env all 

let defaultEnvironment: Environment = (fun s -> None) |> add