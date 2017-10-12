module Lib

open Abstractions
open Args
open Env

// Helpers
let inline binary get op: Function =
    let labels = [Atom (Ref "x"); Atom (Ref "y")] |> Utils.toTree
    let body = fun e ->
        let x, y = get "x" e, get "y" e
        Atom(op x y)
    (labels, body)
let binaryInt op = binary Args.int (fun x y -> Int(op x y))
let binaryBool op = binary Args.bool (fun x y -> Bool(op x y))

// Standard library.
let identity: Function =
    let labels = [Atom (Ref "x")] |> Utils.toTree
    let body = fun a -> a "x"
    (labels, body)

let addInt = binaryInt (+)
let subtractInt = binaryInt (-)
let multiplyInt = binaryInt (*)
let divideInt = binaryInt (/)
let andBool = binaryBool (&&)
let orBool = binaryBool (||)
let equals = binary Args.value (fun x y -> Bool (x = y))
let greaterThan = binary Args.int (fun x y -> Bool (x > y))
let lessThan = binary Args.int (fun x y -> Bool (x < y))

let str: Function =
    let labels = [Atom (Ref "x")] |> Utils.toTree
    let body = fun (a: Arguments) -> 
        a "x" |> Utils.treeToString |> Str |> Atom
    (labels, body)

let ifelse: Function = 
    let labels = [Atom(Ref "pred"); Atom(Ref "iftrue"); Atom(Ref "iffalse")] |> Utils.toTree
    let body = fun (a: Arguments) -> 
        if Args.bool "pred" a then a "iftrue" else a "iffalse"
    (labels, body)

let print: Function =
    let labels = [Atom(Ref "val")] |> Utils.toTree
    let body = (fun (a: Arguments) -> 
        a "val" |> Utils.treeToString |> printfn "%s" |> ignore 
        Atom(Nil))
    (labels, body)
    
let all = [
    ("id", identity);
    ("+", addInt);
    ("*", multiplyInt);
    ("/", divideInt);
    ("-", subtractInt);
    ("=", equals);
    (">", greaterThan);
    ("<", lessThan);
    ("&", andBool);
    ("|", orBool);
    ("str", str);
    ("if", ifelse);
    ("print", print)]

let add (env: Environment) = 
    List.fold (fun e (label, body) -> Env.add label (Variable.Function body) e) env all 