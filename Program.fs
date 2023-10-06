open System.Diagnostics

open FSharpObj
[<EntryPoint>]
let main _ =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    let mesh =
        "stanford-bunny.obj"
        |> Parser.ParseOBJ
        |> Processor.RecalculateNormal
    printfn $"Parsing mesh cost {stopwatch.Elapsed.TotalSeconds} seconds"
    0
