module Lib

open Abstractions
open Args
open Env

// Standard library.

let identity: Function =
    let labels = [Atom (Ref "x")] |> Utils.toTree
    let body = fun a -> a "x"
    (labels, body)

let addInt: Function = 
    let labels = [Atom (Ref "x"); Atom (Ref "y")] |> Utils.toTree
    let body = fun e -> 
        Atom (
            Int(Args.int "x" e + Args.int "y" e))
    (labels, body)

let str: Function =
    let labels = [Atom (Ref "x")] |> Utils.toTree
    let body = fun (a: Arguments) -> 
        a "x" |> Utils.treeToString |> Str |> Atom
    (labels, body)


let all = [
    ("id", identity);
    ("+", addInt);
    ("str", str)]

let add (env: Environment) = 
    List.fold (fun e (label, body) -> Env.add label (Variable.Function body) e) env all 