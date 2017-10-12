module Eval

open Abstractions
open Parser
open Env

let invoke (func:Function) (env:Environment) (values:Tree) = 
    let labels, builder = func
    let argHash = Env.toArgs env
    let result = builder argHash
    result

let rec run (env: Environment) = function
    | Atom(v) -> Atom(v)
    | Tree(Atom(Ref id), right) -> // Special case - function invocation.
        let variable = Env.get env id
        match variable with
        | Function (f) -> invoke f env right
        | _ -> failwith ""
    | Tree(left, right) -> Tree (run env left, run env right)

// Tree(
//    Tree(
//         Atom (Ref "_+"), 
//          Tree (
//             Atom (Ref "x"), 
//             Tree (
//                 Atom (Ref "y"), 
//                 Atom Nil))),
//    Tree (
//         Atom (Ref "x"), 
//         Tree (
//             Atom (Ref "y"), 
//             Atom Nil)))