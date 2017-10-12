module Parser

open Abstractions
open Lexer
open System.Text.RegularExpressions

// Gets the next s-expression in the list, returning
//  the remainder of the list as the second parametere.
let next list = 
    let rec loop depth index = function
        | OpenB :: tail -> loop (depth + 1) (index + 1) tail 
        | CloseB :: tail -> 
            if depth = 1 
            then index
            else loop (depth - 1) (index + 1) tail
        | Symb s :: tail -> 
            if depth = 0
            then index
            else loop depth (index + 1) tail 
        | [] -> if depth = 0 then index else -1

    let index = loop 0 0 list
    if index = -1 then failwith (sprintf "Cant balance: %O" list)
    (List.take (index + 1) list, List.skip (index + 1) list)

let parseValue s = 
    if Regex.IsMatch(s, @"^\d$") then Int(System.Int32.Parse(s)) else
    if Regex.IsMatch(s, @"\bTrue\b") then Bool(System.Boolean.Parse(s)) else
    if Regex.IsMatch(s, "\'.+\'") then Str(s.Trim('''))
    else Ref(s)

// Parses a list of lex tokens into an S-expression.
let rec parse = function 
    | [] -> Atom Nil
    | [Symb s] -> parseValue s |> Atom
    | [OpenB; CloseB] -> Atom Nil
    | OpenB :: tail -> 
        let (next, rest) = next tail
        let remainder = OpenB :: rest
        Tree(parse next, parse remainder)
    | _ -> failwith "invalid tree"

// Ensures that brackets are properly balanced.
let balanced tokens = 
    let rec loop depth = function
        | Token.CloseB :: _  when depth = 0 -> false
        | Token.OpenB :: tail -> loop (depth + 1) tail
        | Token.CloseB :: tail -> loop (depth - 1) tail
        | __ :: tail -> loop (depth) tail
        | [] -> depth = 0
        | _::_ -> false

    loop 0 tokens
    