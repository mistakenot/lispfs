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

let define id args body env = 
    match args, body with
    | Atom(var), Atom(Nil) -> Env.add id (Variable.Value(Atom(var))) env
    | Tree(argl, argr), body -> Env.add id (Variable.Function(Tree(argl, argr), fun a -> body)) env
    | _ -> failwith "Incorrect definition."

// These active patterns describe the operations
// that can be executed.
let (|Invocation|Definition|Expression|) = function
    | Tree(Atom(Ref "def"), Tree(Atom(Ref id), args)) -> Definition(id, args)
    | Tree(Atom(Ref id), args) -> Invocation(id, args)
    | e -> Expression e


let rec run (env: Environment) = function
    | Invocation(id, args) -> 
        let argValues = run env args
        match Env.get env id with
        | Function (f) -> 
            (invoke f env argValues |> run env)
        | Value(t) -> Tree(t, args)
    | Definition(id, args) -> Atom Nil
    | Expression e -> 
        match e with
        | Atom v -> Atom v
        | Tree(l, r) -> Tree (run env l, run env r)

// Evaluates the tree against the environment
// let rec run (env: Environment) = function
//     | Atom(v) -> Atom(v)
//     | Tree(Atom(Ref id), r) -> 
//         // Function invocation
//         let variable = Env.get env id
//         let args = run env r
//         match variable with
//         | Function (f) -> invoke f env args |> run env
//         | Value(t) -> Tree(t, args)
//     | Tree(left, right) -> Tree (run env left, run env right)