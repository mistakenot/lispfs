module EvalTests

open Eval
open Parser
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Eval atom returns atom`` () = 
    let actual = Atom(Int 0) |> Eval.run
    let expected = Atom(Int 0)
    actual |> should equal expected

[<Fact>]
let ``Eval tree of (Atom, Nil)`` () = 
    let actual = Tree(Atom(Int 0), Atom(Nil)) |> Eval.run
    let expected = Atom(Int 0)
    actual |> should equal expected

[<Fact>]
let ``Eval tree of (Atom, Atom), no environment`` () =
    let actual = Tree(Atom(Int 0), Atom(Int 1)) |> Eval.run
    let expected = Tree(Atom(Int 0), Atom(Int 1))
    actual |> should equal expected
    