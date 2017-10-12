module Lisp

open Abstractions
open Env

let defaultEnvironment: Environment = (fun s -> None) |> Lib.add