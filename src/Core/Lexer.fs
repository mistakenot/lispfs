module Lexer

    type Token = 
        | Symb of string
        | OpenB
        | CloseB

    // Given a list of symbols starting with a bracket, 
    //  this will give you the contents of that bracket as
    //  the first return, and the remainder as the second.
    // E.g. (1(23))456 -> (1(23)), 456
    let contentsFrom list = 
        let rec loop depth index = function
            | OpenB :: tail -> loop (depth + 1) (index + 1) tail 
            | CloseB :: tail -> 
                if depth = 1 
                then index
                else loop (depth - 1) (index + 1) tail
            | head::tail -> loop depth (index + 1) tail 
            | [] -> if depth = 0 then index else failwith "not balanced"

        match list with
        | OpenB :: tail -> 
            let index = loop 1 0 tail
            (List.take index tail, List.skip (index + 1) tail)
        | _ -> failwith "not balanced"

    let tokenise: string -> Token list  = 

        let identToToken = Seq.map (sprintf "%c") >> String.concat "" >> Token.Symb

        let rec loop (currentSymb: char list option)  (accum: Token list) (str: char list) = 
            match currentSymb with
            | Some s ->
                match str with
                | '(' :: tail -> loop None (accum @ [identToToken s; Token.OpenB]) tail
                | ')' :: tail -> loop None (accum @ [identToToken s; Token.CloseB]) tail
                | ' ' :: tail -> loop None (accum @ [identToToken s]) tail
                | c :: tail   -> loop (s @ [c] |> Some) accum tail
                | [] -> identToToken s :: accum
            | None -> 
                match str with
                | '(' :: tail -> loop None (accum @ [Token.OpenB]) tail
                | ')' :: tail -> loop None (accum @ [Token.CloseB]) tail
                | ' ' :: tail -> loop None (accum) tail
                | c :: tail   -> loop (Some([c])) accum tail
                | [] -> accum

        Seq.toList >> loop None []