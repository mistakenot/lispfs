module InterpTests

open Interp
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Interprets simple value`` () = 
    run "1" |> should equal "1"

[<Fact>]
let ``Interprets singleton list`` () = 
    run "(1)" |> should equal "(1)"

[<Fact>]
let ``Interprets atom tuple`` () = 
    run "(1 2)" |> should equal "(1 (2))"

[<Fact>]
let ``Interprets atom list`` () = 
    run "(1 2 3)" |> should equal "(1 (2 (3)))"

[<Fact>]
let ``Interprets identity function`` () =
    run "(id 1)" |> should equal "1"

[<Fact>]
let ``Interprets addition function`` () =
    run "(+ 1 2)" |> should equal "3"

[<Fact>]
let ``Inerprets nested function`` () =
    run "(+ (+ 1 2) 3)" |> should equal "6"