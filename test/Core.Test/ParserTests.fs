module ParserTests

open Parser
open Lexer
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Next can get atom`` () = 
    let (next, rest) = [Symb "0"; Symb "1"] |> Parser.next
    next |> should equal [Symb "0"]
    rest |> should equal [Symb "1"]

[<Fact>]
let ``Next can get tree`` () = 
    let (next, rest) = [OpenB; Symb "0"; CloseB; Symb "1"] |> Parser.next
    next |> should equal [OpenB; Symb "0"; CloseB]
    rest |> should equal [Symb "1"]

[<Fact>]
let ``Parse nil`` () = 
    let actual = [] |> Parser.parse
    actual |> should equal Nil

[<Fact>]    
let ``Parse symbol`` () = 
    let actual = [Symb "0"] |> Parser.parse
    actual |> should equal (Atom "0")

[<Fact>]
let ``Parse simple tree`` () = 
    // (0)
    let actual = [OpenB; Symb "0"; CloseB] |> Parser.parse
    actual |> should equal (Tree(Atom "0", Nil))

[<Fact>]
let ``Parse multiple atoms`` () = 
    // (0 1)
    let actual = [OpenB; Symb "0"; Symb "1"; CloseB] |> Parser.parse
    actual |> should equal (Tree(Atom "0", Tree(Atom "1", Nil)))

[<Fact>]
let ``Parse nested simple tree`` () = 
    // (0 (1))
    let actual = [OpenB; Symb "0"; OpenB; Symb "1"; CloseB; CloseB] |> Parser.parse
    actual |> should equal (
        Tree(
            Atom "0", 
            Tree(
                Tree(Atom "1", Nil), 
                Nil)))

[<Fact>]
let ``Parse nested multiple atoms`` () = 
    // (0 (1 2)) -> (0 ((1(2))))
    let actual = [OpenB; Symb "0"; OpenB; Symb "1"; Symb "2"; CloseB; CloseB] |> Parser.parse
    actual |> should equal (
        Tree(
            Atom "0", 
            Tree(
                Tree(
                    Atom "1", 
                    Tree(
                        Atom "2",
                        Nil)), 
                Nil)))

[<Fact>]
let ``Parse nested multiple symbols and atoms`` () = 
    // (* 2 (+ 3 4))
    let actual = [OpenB; Symb "*"; Symb "2"; OpenB; Symb "+"; Symb "3"; Symb "4"; CloseB; CloseB] |> Parser.parse
    let expected = 
        Tree(
            Atom "*", 
            Tree(
                Atom "2", 
                Tree(
                    Tree(
                        Atom("+"), 
                        Tree(
                            Atom("3"),
                            Tree(
                                Atom "4", 
                                Nil)))
                    , Nil)))
    actual |> should equal expected