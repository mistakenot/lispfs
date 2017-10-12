module LexerTests

open System
open Xunit
open Lexer
open FsUnit.Xunit

[<Fact>]
let ``Parse value`` () = 
    let statement = "1"
    let actual = Lexer.tokenise statement
    let expected = [Symb "1"]
    actual |> should equal expected
    
[<Fact>]
let ``Parse empty list`` () = 
    let statement = "()"
    let actual = Lexer.tokenise statement
    let expected = [OpenB; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse integer constant`` () = 
    let statement = "(1)"
    let actual = Lexer.tokenise statement
    let expected = [OpenB; Symb "1"; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse string constant`` () = 
    let actual = Lexer.tokenise "(abc)"
    let expected = [OpenB; Symb "abc"; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse nested string constant`` () = 
    let actual = Lexer.tokenise "(abc (1))"
    let expected = [OpenB; Symb "abc"; OpenB;  Symb "1"; CloseB; CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse multiple string identifiers`` () = 
    let actual = Lexer.tokenise "(a b c)"
    let expected = [OpenB; Symb "a"; Symb "b";  Symb "c"; CloseB;]
    actual |> should equal expected

[<Fact>]
let ``Parse multiple and nested identifiers`` () = 
    let actual = Lexer.tokenise "(* 2 (+ 3 4))"
    let expected = [OpenB; Symb "*"; Symb "2"; OpenB; Symb "+"; Symb "3"; Symb "4"; CloseB; CloseB;]
    actual |> should equal expected