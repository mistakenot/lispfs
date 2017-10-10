module LexerTests

open System
open Xunit
open Lexer
open FsUnit.Xunit

[<Fact>]
let ``Parse basic statement`` () = 
    let statement = "()"
    let actual = Lexer.tokenise statement
    let expected = [Token.OpenB; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse integer constant`` () = 
    let statement = "(1)"
    let actual = Lexer.tokenise statement
    let expected = [Token.OpenB; Token.Identifier "1"; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse string constant`` () = 
    let statement = "(abc)"
    let actual = Lexer.tokenise statement
    let expected = [Token.OpenB; Token.Identifier "abc"; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse nested string constant`` () = 
    let statement = "(abc (1) )"
    let actual = Lexer.tokenise statement
    let expected = [Token.OpenB; Token.Identifier "abc"; Token.OpenB; Token.Identifier "1"; Token.CloseB; Token.CloseB]
    actual |> should equal expected

[<Fact>]
let ``Parse multiple string identifiers`` () = 
    let statement = "(a b c)"
    let actual = Lexer.tokenise statement
    let expected = [Token.OpenB; Token.Identifier "a"; Token.Identifier "b"; Token.Identifier "c"; Token.CloseB]
    actual |> should equal expected