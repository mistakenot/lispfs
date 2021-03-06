module UtilsTests

open Abstractions
open Utils
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Int atom to string`` () = 
    let expected = "0"
    let actual = Atom(Int 0) |> treeToString
    actual |> should equal expected

[<Fact>]
let ``Nil atom to string`` () = 
    let expected = ""
    let actual = Atom(Nil) |> treeToString
    actual |> should equal expected

[<Fact>]
let ``Bool atom to string`` () = 
    let expected = "True"
    let actual = Atom(Bool true) |> treeToString
    actual |> should equal expected

[<Fact>]
let ``String atom to string`` () = 
    let expected = "'test'"
    let actual = Atom(Str "test") |> treeToString
    actual |> should equal expected

[<Fact>]
let ``Ref atom to string`` () = 
    let expected = "x"
    let actual = Atom(Ref "x") |> treeToString
    actual |> should equal expected

[<Fact>]
let ``Atom toTree`` () = 
    let expected = Tree(Atom(Int 0), Atom Nil)
    let actual = [Atom(Int 0)] |> toTree
    actual |> should equal expected

[<Fact>]
let ``Trees to tree`` () = 
    let expected = 
        Tree(
            Atom(Int 0),
            Tree(
                Atom(Int 1),
                Atom Nil))
    let actual = [Atom(Int 0); Atom(Int 1)] |> toTree
    actual |> should equal expected