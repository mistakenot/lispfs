module Lexer

    type Token = 
        | Identifier of string
        | OpenB
        | CloseB

    let tokenise (s: string) = 
        let readToken list = 
            let loop accum = function
                | '(' :: tail -> (accum, '(' :: tail)
                | ')' :: tail -> (accum, '(' :: tail)                
                | [] -> (accum, [])
                | c :: tail -> (accum, '(' :: tail)
            loop [] list 

        let identToToken = Seq.map (sprintf "%c") >> String.concat "" >> Token.Identifier
        let rec loop (currentIdentifier: char list option)  (accum: Token list) (str: char list) = 
            match currentIdentifier with
            | Some s ->
                match str with
                | '(' :: tail -> loop None (accum @ [identToToken s; Token.OpenB]) tail
                | ')' :: tail -> loop None (accum @ [identToToken s; Token.CloseB]) tail
                | ' ' :: tail -> loop None (accum @ [identToToken s]) tail
                | c :: tail   -> loop (s @ [c] |> Some) accum tail
                | [] -> accum
            | None -> 
                match str with
                | '(' :: tail -> loop None (accum @ [Token.OpenB]) tail
                | ')' :: tail -> loop None (accum @ [Token.CloseB]) tail
                | ' ' :: tail -> loop None (accum) tail
                | c :: tail   -> loop (Some([c])) accum tail
                | [] -> accum

        Seq.toList s |> loop None []