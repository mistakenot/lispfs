module Utils

open Abstractions

let valueToString = function
    | Nil -> ""
    | Int i -> i.ToString()
    | Bool b -> b.ToString()
    | Str s -> sprintf "'%s'" s
    | Ref r -> r

let rec treeToString (tree: Tree) = 
    match tree with
    | Atom(v) -> valueToString v
    | Tree(l, Atom Nil) -> sprintf "(%s)" (treeToString l)
    | Tree(l, r) -> sprintf "(%s %s)" (treeToString l) (treeToString r)