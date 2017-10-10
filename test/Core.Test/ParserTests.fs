module ParserTests

open Parser
open Lexer
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Can parse single symbol`` () = 
    // (1)
    let actual = [OpenB; Identifier "1"; CloseB] |> Parser.parse
    let expected = Node(Symbol "1", Nil)
    actual |> should equal expected

[<Fact>]
let ``Can parse multiple symbols`` () = 
    // (+ 1 2)
    let actual = [OpenB; Identifier "+"; Identifier "1"; Identifier "2"; CloseB] |> Parser.parse
    let expected = Node(Symbol "+", Node(Symbol "1", Node(Symbol "2", Nil)))
    actual |> should equal expected

[<Fact>]
let ``Can preprocess multiple symbols`` () = 
    // (+ 1 2)
    let actual = [OpenB; Identifier "+"; Identifier "1"; Identifier "2"; CloseB] |> Parser.preprocess
    let expected = [OpenB; Identifier "+"; OpenB; Identifier "1"; OpenB; Identifier "2"; CloseB; CloseB; CloseB;]
    actual |> should equal expected

let ``Can parse nested symbols`` () = 
    // (* 2 (+ 3 4))
    let actual = [OpenB; Identifier "*"; Identifier "2"; OpenB; Identifier "+"; Identifier "3"; Identifier "4"; CloseB; CloseB] |> Parser.parse
    let expected = 
        Node(
            Symbol "*", 
            Node(
                Symbol "2", 
                Node(
                    Node(
                        Symbol("+"), 
                        Node(
                            Symbol("3"),
                            Node(
                                Symbol "4", 
                                Nil)))
                    , Nil)))
    actual |> should equal expected

[<Fact>]
let ``Get enclosed list 2`` () = 
    let actual = [OpenB; Identifier "*"; OpenB; Identifier "1"; CloseB; CloseB;] |> Parser.getEnclosedList
    let (a,b) = actual
    a |> should equal [Identifier "*"; OpenB; Identifier "1"; CloseB;]
    b.IsEmpty |> should equal true

[<Fact>]
let ``Get enclosed list 3`` () =    
    let actual = [OpenB; Identifier "1"; CloseB; Identifier "2"]
    let (a, b) = actual |> Parser.getEnclosedList
    a |> should equal [Identifier "1"]
    b |> should equal [Identifier "2"]

// [<Fact>]
// let ``Get enclosed list 3`` () = 
//     let actual = [Identifier "*"; OpenB; Identifier "1"; CloseB;] 
//     actual |> Parser.getEnclosedList |> Parser.getEnclosedList|> should equal [Identifier "1";]

[<Fact>]
let ``Preprocess`` () = 
    let actual = [OpenB; Identifier "1"; Identifier "2"; CloseB] |> Parser.preprocess
    let expected = [OpenB; Identifier "1"; OpenB; Identifier "2"; CloseB; CloseB]
    actual |> should equal expected