module Interp

open Abstractions

let defaultEnvironment: Environment = (fun s -> None) |> Lib.add

let lex = Lexer.tokenise

let parse = Parser.parse

let eval = Eval.run defaultEnvironment

let print = Utils.treeToString

let run = lex >> parse >> eval >> print