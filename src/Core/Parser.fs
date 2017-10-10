module Parser

open Lexer
open Types

type Tree =
    | Nil
    | Symbol of string
    | Node of (Tree * Tree)

let rec preprocess = function
    | Identifier a :: Identifier b :: tail 
        -> Identifier a :: OpenB :: (preprocess (Identifier b :: tail) @ [CloseB])
    | head :: tail 
        -> head :: preprocess tail 
    | [] -> []

let getEnclosedList list = 
    let rec loop depth index = function
        | OpenB :: tail -> loop (depth + 1) (index + 1) tail 
        | CloseB :: tail -> 
            if depth = 1 
            then index
            else loop (depth - 1) (index + 1) tail
        | head::tail -> loop depth (index + 1) tail 
        | [] -> if depth = 0 then index else failwith "not balanced"

    match list with
    | OpenB :: tail -> 
        let index = loop 1 0 tail
        (List.take index tail, List.skip (index + 1) tail)
    | _ -> failwith "not balanced"

let parse tokens = 
    let balanced tokens = 
        let rec loop depth = function
            | Token.CloseB :: _  when depth = 0 -> false
            | Token.OpenB :: tail -> loop (depth + 1) tail
            | Token.CloseB :: tail -> loop (depth - 1) tail
            | __ :: tail -> loop (depth) tail
            | [] -> depth = 0
            | _::_ -> false

        loop 0 tokens
    
    if not (balanced tokens) then failwith "not balanced" 

    let rec loop = function
        | OpenB :: tail -> 
            let (enclosed, remainder) = OpenB :: tail |> getEnclosedList
            Node(loop enclosed, loop remainder)
        | [Identifier i] -> Symbol(i)
        | [CloseB] -> Nil
        | [] -> Nil
        | _ -> failwith "no match"

    preprocess tokens |> loop
        