module Lib

open Types
open Parser
open Env

let identity =
    let labels = Atom (Ref "x")
    let body = Atom (Ref "x")
    ("id", Tree(body, labels))

let addIntsNative = 
    let labels = 
        Tree(
            Atom(Ref "x"),
            Tree(
                Atom(Ref "y"),
                Atom(Nil)))
    let factory = (fun env -> 
        let x, y = Env.getInt "x" env, Env.getInt "y" env
        x + y |> Int |> Atom )
    ("_+", factory)

let addInts = 
    let labels = 
        Tree(
            Atom (Ref "x"),
            Tree (
                Atom (Ref "y"),
                Atom (Nil)))
    let body = 
        Tree(
            Atom (Ref "_+"),
            labels)
    ("+", Tree(body, labels))

let all = [identity; addInts]

let native = [addIntsNative]

let add (env: Environment) = 
    let a = List.fold (fun e (label, body) -> Env.add label body e) env all 
    List.fold (fun e (label, factory) -> Env.addNative label factory e) a native