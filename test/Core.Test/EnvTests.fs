module EnvTests

open Abstractions
open Env
open Lib
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Get nonexistent ref returns none`` () = 
    defaultEnvironment "na" |> should equal None

[<Fact>]
let ``Get existent ref returns value`` () = 
    let env = Env.add "ref" (Variable.Value(Atom(Int 0))) defaultEnvironment
    env "ref" |> should equal (Some(Value(Atom(Int 0))))

[<Fact>]
let ``Get returns most recent value`` () = 
    let env = defaultEnvironment |> Env.add "ref" (Value(Atom(Int 0))) |> Env.add "ref" (Variable.Value(Atom(Int 1)))
    env "ref" |> should equal (Some(Variable.Value(Atom(Int 1))))

[<Fact>]
let ``Fold over empty arg list returns empty env`` () =
    let env = Env.foldArgs (Atom Nil) (Atom Nil) defaultEnvironment
    env "na" |> should equal None

[<Fact>]
let ``Fold over single arg returns arg when called`` () = 
    let labels = Tree(Atom(Ref "x"), Atom Nil)
    let values = Tree(Atom(Int 0), Atom Nil)
    let env = Env.foldArgs labels values defaultEnvironment
    env "x" |> should equal (Some(Value(Atom(Int 0))))

[<Fact>]
let ``Fold over multi args returns args when called`` () = 
    let labels = 
        Tree(
            Atom(Ref "x"), 
            Tree(
                Atom(Ref "y"),
                Atom(Nil)))       
    let values = 
        Tree(
            Atom(Int 0), 
            Tree(
                Atom(Int 1),
                Atom(Nil)))
    let env = Env.foldArgs labels values defaultEnvironment
    env "x" |> should equal (Some(Value(Atom(Int 0))))
    env "y" |> should equal (Some(Value(Atom(Int 1))))