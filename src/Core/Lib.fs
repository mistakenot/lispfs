module Lib

open Types

let addX x = (fun input -> function 
    | Int y -> x + y |> Int
    | _ -> failwith "Incorrect parameters." )

let add = function
    | Int(x) -> addX x
    | _ -> failwith "Incorrect parameters"

let subtract = function
    | Int(x) -> addX -x
    | _ -> failwith "Incorrect parameters"

let multiply = function
    | Int(x) -> (fun input -> function
        | Int(y) -> x * y |> Int
        | _ -> failwith "Incorrect parameters." )
    | _ -> failwith "Incorrect parameters."