module Abstractions

// Value types.
type Value =
    | Nil 
    | Int of int
    | Bool of bool
    | Str of string
    | Ref of string

// S-Expression definition.
type Tree =
    | Atom of Value
    | Tree of (Tree * Tree)

// Value hash.
type Arguments = string -> Tree

// Native Function definition.
// (signature, body)
type Function = (Tree * (Arguments -> Tree))

// Two types of registerable variables.
type Variable = 
    | Value of Tree 
    | Function of Function

// Execution environment.
type Environment = string -> Variable option