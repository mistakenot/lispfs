module Interpreter

open Lib
open Abstractions

let defaultEnvironment: Environment = (fun s -> None) |> add

let lex = Lexer.tokenise

let parse = Parser.parse

let eval = Eval.run defaultEnvironment

let print = sprintf "%O"

let run = lex >> parse >> eval >> print