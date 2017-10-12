module EvalTests

open Abstractions
open Eval
open Interp
open Parser
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Eval atom returns atom`` () = 
    let actual = Atom(Int 0) |> eval
    let expected = Atom(Int 0)
    actual |> should equal expected

[<Fact>]
let ``Eval tree of (Atom, Nil)`` () = 
    let actual = Tree(Atom(Int 0), Atom(Nil)) |> eval
    let expected = Tree(Atom(Int 0), Atom(Nil))
    actual |> should equal expected

[<Fact>]
let ``Eval tree of (atom, atom), no environment`` () =
    let actual = Tree(Atom(Int 0), Atom(Int 1)) |> eval
    let expected = Tree(Atom(Int 0), Atom(Int 1))
    actual |> should equal expected

[<Fact>]
let ``Invoke identity function`` () = 
    let values = 
        Tree(
            Atom(Int 0),
            Atom Nil)
    let expected = Atom(Int 0)
    let actual = Eval.invoke Lib.identity defaultEnvironment values
    actual |> should equal expected

[<Fact>]
let ``Invoke int addition`` () = 
    let values = 
        Tree(
            Atom(Int 1),
            Tree(
                Atom(Int 2),
                Atom Nil))
    let expected = Atom(Int 3)
    let actual = Eval.invoke Lib.addInt defaultEnvironment values
    actual |> should equal expected            

[<Fact>]
let ``Eval reference to value`` () = 
    let var = Variable.Value(Atom(Int 0))
    let env = Env.add "x" var defaultEnvironment
    let actual = Tree(Atom (Ref "x"), Atom(Nil)) |> Eval.run env
    let expected = Tree(Atom(Int 0), Atom Nil)
    actual |> should equal expected

[<Fact>]
let ``Eval identity function`` () = 
    let actual = 
        Tree(
            Atom(Ref "id"),
            Tree(
                Atom(Int 0), 
                Atom Nil)) |> eval
    let expected = Atom (Int 0)
    actual |> should equal expected

[<Fact>]
let ``Eval addition function`` () = 
    let actual = 
        Tree(
            Atom(Ref "+"), 
            Tree(
                Atom(Int 1),
                Tree(
                    Atom(Int 2),
                    Atom(Nil)))) |> eval
    let expected = Atom (Int 3)            
    actual |> should equal expected
