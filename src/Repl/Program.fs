// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn " Lispfs Repl"
    printfn " Refer to http://github.com/mistakenot/listfs for docs."
    printfn " Enter q to quit."

    while true do
        Console.Write("> ")
        let input = Console.ReadLine()
        if input = "q" then failwith "Exited by user." 
        try        
            let output = Interp.run input
            Console.WriteLine(output)
        with
        | e -> Console.WriteLine(e.Message) |> ignore
    0 // return an integer exit code
