module LibTests

open Abstractions
open Interp
open Xunit
open FsUnit.Xunit

let invoke func args = 
    Utils.toTree args |> Eval.invoke func defaultEnvironment

[<Fact>]
let ``Invoke identity`` () = 
    let args = [Atom(Int 0)]
    let actual = invoke Lib.identity args
    let expected = Atom(Int 0)
    actual |> should equal expected

[<Fact>]
let ``Invoke str`` () =
    let args = [Atom(Int 0)]
    let actual = invoke Lib.str args
    let expected = Atom(Str "0")
    actual |> should equal expected

[<Fact>]
let ``Invoke ifelse with true atom`` () = 
    let args = [Atom(Bool true); Atom(Int 1); Atom(Int 0)]
    let actual = invoke Lib.ifelse args
    let expected = Atom(Int 1)
    actual |> should equal expected

[<Fact>]
let ``Invoke ifelse with false atom`` () = 
    let args = [Atom(Bool false); Atom(Int 1); Atom(Int 0)]
    let actual = invoke Lib.ifelse args
    let expected = Atom(Int 0)
    actual |> should equal expected

[<Fact>]
let ``Invoke multiply`` () =
    let args = [Atom(Int 2); Atom(Int 3)]
    let actual = invoke Lib.multiplyInt args
    let expected = Atom(Int 6)
    actual |> should equal expected

[<Fact>]
let ``Invoke divide`` () =  
    let args = [Atom(Int 6); Atom(Int 3)]
    let actual = invoke Lib.divideInt args
    let expected = Atom(Int 2)
    actual |> should equal expected 

[<Fact>]
let ``Invoke add`` () =
    let args = [Atom(Int 2); Atom(Int 2)]
    let actual = invoke Lib.addInt args
    let expected = Atom(Int 4)
    actual |> should equal expected

[<Fact>]
let ``Invoke subtract`` () =
    let args = [Atom(Int 2); Atom(Int 2)]
    let actual = invoke Lib.subtractInt args
    let expected = Atom(Int 0)
    actual |> should equal expected

[<Fact>]
let ``Invoke equals on ints`` () =
    let args = [Atom(Int 2); Atom(Int 2)]
    let actual = invoke Lib.equals args
    let expected = Atom(Bool true)
    actual |> should equal expected

[<Fact>]
let ``Invoke equals on strings`` () =
    let args = [Atom(Str "1"); Atom(Str "1")]
    let actual = invoke Lib.equals args
    let expected = Atom(Bool true)
    actual |> should equal expected

[<Fact>]
let ``Invoke greater than`` () =
    let args = [Atom(Int 2); Atom(Int 1)]
    let actual = invoke Lib.greaterThan args
    let expected = Atom(Bool true)
    actual |> should equal expected

[<Fact>]
let ``Invoke less than`` () =
    let args = [Atom(Int 1); Atom(Int 2)]
    let actual = invoke Lib.lessThan args
    let expected = Atom(Bool true)
    actual |> should equal expected

[<Fact>]
let ``Invoke and`` () =
    let args = [Atom(Bool true); Atom(Bool true)]
    let actual = invoke Lib.andBool args
    let expected = Atom(Bool true)
    actual |> should equal expected

[<Fact>]
let ``Invoke or`` () =
    let args = [Atom(Bool true); Atom(Bool true)]
    let actual = invoke Lib.orBool args
    let expected = Atom(Bool true)
    actual |> should equal expected