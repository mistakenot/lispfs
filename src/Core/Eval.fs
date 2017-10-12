module Eval

open Parser
open Env

let defaultEnvironment: Environment = (fun s -> None) |> Lib.add

let rec run (env: Environment) = function
    | Atom(v) -> Atom(v)
    // Dereferencing
    // | Tree(Atom(Ref id), r) -> 
    //     // failwith (sprintf "%O" (run env (Atom(Ref id))))
    //     let right = run env r           // Get our arg values
    //     match env id with
    //     | Some(Atom v) -> Tree(Atom v, right) // Refers to a value
    //     | Some(Tree (body, argLabels)) ->         // Refers to a function or a list of values
    //         let nextEnv = Env.foldArgs argLabels argValues env
    //         run nextEnv body
    //     | None -> sprintf "Identifier %s not found." id |> failwith
    | Tree(Atom(Ref id), right) -> 
        match env id with
        | Some tree -> 
            match tree with 
            | Atom v -> failwith (sprintf "ATOM %O" tree)
            | Tree(body, labels) -> 
                let args = run env right
                let nextEnv = Env.foldArgs labels args env
                
                run nextEnv body
                
        | None -> sprintf "Identifier %s not found." id |> failwith

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