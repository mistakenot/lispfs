module Eval

open Abstractions
open Parser
open Env

// Invokes a function
let invoke (func:Function) (env:Environment) (values:Tree) = 
    let labels, builder = func
    let argHash = Env.foldArgs labels values env |> Env.toArgs
    let result = builder argHash
    result

// Evaluates the tree against the environment
let rec run (env: Environment) = function
    | Atom(v) -> Atom(v)
    | Tree(Atom(Ref id), r) -> 
        // Function invocation
        let variable = Env.get env id
        let args = run env r
        match variable with
        | Function (f) -> invoke f env args |> run env
        | Value(t) -> Tree(t, args)
    | Tree(left, right) -> Tree (run env left, run env right)